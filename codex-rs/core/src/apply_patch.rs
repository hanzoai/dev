use crate::codex::Session;
use crate::patch_harness::run_patch_harness;
use codex_protocol::models::FunctionCallOutputPayload;
use codex_protocol::models::ResponseInputItem;
use crate::protocol::FileChange;
use crate::protocol::ReviewDecision;
use crate::safety::SafetyCheck;
use crate::safety::assess_patch_safety;
use codex_apply_patch::ApplyPatchAction;
use codex_apply_patch::ApplyPatchFileChange;
use serde_json::json;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

pub const CODEX_APPLY_PATCH_ARG1: &str = "--codex-run-as-apply-patch";

pub(crate) enum InternalApplyPatchInvocation {
    /// The `apply_patch` call was handled programmatically, without any sort
    /// of sandbox, because the user explicitly approved it. This is the
    /// result to use with the `shell` function call that contained `apply_patch`.
    Output(ResponseInputItem),

    /// The `apply_patch` call was approved, either automatically because it
    /// appears that it should be allowed based on the user's sandbox policy
    /// *or* because the user explicitly approved it. In either case, we use
    /// exec with [`CODEX_APPLY_PATCH_ARG1`] to realize the `apply_patch` call,
    /// but [`ApplyPatchExec::auto_approved`] is used to determine the sandbox
    /// used with the `exec()`.
    DelegateToExec(ApplyPatchExec),
}

pub(crate) struct ApplyPatchExec {
    pub(crate) action: ApplyPatchAction,
    pub(crate) user_explicitly_approved_this_action: bool,
    pub(crate) harness_summary_json: Option<String>,
}

impl From<ResponseInputItem> for InternalApplyPatchInvocation {
    fn from(item: ResponseInputItem) -> Self {
        InternalApplyPatchInvocation::Output(item)
    }
}

pub(crate) async fn apply_patch(
    sess: &Session,
    sub_id: &str,
    call_id: &str,
    action: ApplyPatchAction,
) -> InternalApplyPatchInvocation {
    // Run the validation harness before assessing safety so we can surface a
    // concise status message and capture structured findings for the model.
    let (harness_summary_json, harness_status_message) = {
        let mut summary_json: Option<String> = None;
        let mut status_message: Option<String> = None;
        let validation_cfg = sess.validation_config();
        let github_cfg = sess.get_github_config();
        if let (Ok(validation_cfg), Ok(github_cfg)) = (validation_cfg.read(), github_cfg.read()) {
            if let Some((mut findings, mut ran_checks)) = run_patch_harness(
                &action,
                sess.get_cwd(),
                &*validation_cfg,
                &*github_cfg,
            ) {
                const MAX_ISSUES: usize = 12;
                let total_issues = findings.len();
                let truncated = total_issues > MAX_ISSUES;
                if truncated {
                    findings.truncate(MAX_ISSUES);
                }
                findings.retain(|finding| {
                    finding.tool.trim().len() <= 120 && finding.message.trim().len() <= 800
                });
                let issues_json: Vec<serde_json::Value> = findings
                    .iter()
                    .map(|finding| {
                        let relative_file = finding
                            .file
                            .as_ref()
                            .and_then(|path| path.strip_prefix(sess.get_cwd()).ok())
                            .map(|path| path.display().to_string());
                        json!({
                            "tool": finding.tool,
                            "file": relative_file,
                            "msg": finding.message,
                        })
                    })
                    .collect();
                summary_json = Some(
                    json!({
                        "validation": {
                            "issues": issues_json,
                            "checks": ran_checks,
                            "issue_count": total_issues,
                            "truncated": truncated,
                        }
                    })
                    .to_string(),
                );

                let mut lines: Vec<String> = Vec::new();
                if total_issues == 0 {
                    lines.push("✅ Validate New Code: no issues".to_string());
                } else {
                    lines.push(format!("❌ Validate New Code: {total_issues} issue(s)"));
                    for finding in findings.iter() {
                        let mut parts = vec![finding.tool.clone()];
                        if let Some(rel) = finding
                            .file
                            .as_ref()
                            .and_then(|p| p.strip_prefix(sess.get_cwd()).ok())
                            .map(|p| p.display().to_string())
                        {
                            parts.push(rel);
                        }
                        let mut msg = finding.message.clone();
                        if msg.len() > 160 {
                            msg.truncate(157);
                            msg.push_str("…");
                        }
                        parts.push(msg);
                        lines.push(format!("• {}", parts.join(" — ")));
                    }
                    if truncated {
                        let remaining = total_issues - findings.len();
                        lines.push(format!("… plus {remaining} more issue(s)"));
                    }
                }
                if ran_checks.is_empty() {
                    lines.push("Checks run: none".to_string());
                } else {
                    ran_checks.sort();
                    lines.push(format!("Checks run: {}", ran_checks.join(", ")));
                }
                status_message = Some(lines.join("\n"));
            }
        }
        (summary_json, status_message)
    };

    if let Some(message) = harness_status_message.as_ref() {
        sess.notify_background_event(sub_id, message.clone()).await;
    }

    match assess_patch_safety(
        &action,
        sess.get_approval_policy(),
        sess.get_sandbox_policy(),
        sess.get_cwd(),
    ) {
        SafetyCheck::AutoApprove { .. } => {
            InternalApplyPatchInvocation::DelegateToExec(ApplyPatchExec {
                action,
                user_explicitly_approved_this_action: false,
                harness_summary_json: harness_summary_json.clone(),
            })
        }
        SafetyCheck::AskUser => {
            // Compute a readable summary of path changes to include in the
            // approval request so the user can make an informed decision.
            //
            // Note that it might be worth expanding this approval request to
            // give the user the option to expand the set of writable roots so
            // that similar patches can be auto-approved in the future during
            // this session.
            let rx_approve = sess
                .request_patch_approval(sub_id.to_owned(), call_id.to_owned(), &action, None, None)
                .await;
            match rx_approve.await.unwrap_or_default() {
                ReviewDecision::Approved | ReviewDecision::ApprovedForSession => {
                    InternalApplyPatchInvocation::DelegateToExec(ApplyPatchExec {
                        action,
                        user_explicitly_approved_this_action: true,
                        harness_summary_json,
                    })
                }
                ReviewDecision::Denied | ReviewDecision::Abort => {
                    ResponseInputItem::FunctionCallOutput {
                        call_id: call_id.to_owned(),
                        output: FunctionCallOutputPayload {
                            content: "patch rejected by user".to_string(),
                            success: Some(false),
                        },
                    }
                    .into()
                }
            }
        }
        SafetyCheck::Reject { reason } => ResponseInputItem::FunctionCallOutput {
            call_id: call_id.to_owned(),
            output: FunctionCallOutputPayload {
                content: format!("patch rejected: {reason}"),
                success: Some(false),
            },
        }
        .into(),
    }
}

pub(crate) fn convert_apply_patch_to_protocol(
    action: &ApplyPatchAction,
) -> HashMap<PathBuf, FileChange> {
    let changes = action.changes();
    let mut result = HashMap::with_capacity(changes.len());
    for (path, change) in changes {
        let protocol_change = match change {
            ApplyPatchFileChange::Add { content } => FileChange::Add {
                content: content.clone(),
            },
            ApplyPatchFileChange::Delete { .. } => FileChange::Delete {},
            ApplyPatchFileChange::Update {
                unified_diff,
                move_path,
                new_content: _new_content,
            } => FileChange::Update {
                unified_diff: unified_diff.clone(),
                move_path: move_path.clone(),
            },
        };
        result.insert(path.clone(), protocol_change);
    }
    result
}

pub(crate) fn get_writable_roots(cwd: &Path) -> Vec<PathBuf> {
    let mut writable_roots = Vec::new();
    if cfg!(target_os = "macos") {
        // On macOS, $TMPDIR is private to the user.
        writable_roots.push(std::env::temp_dir());

        // Allow pyenv to update its shims directory. Without this, any tool
        // that happens to be managed by `pyenv` will fail with an error like:
        //
        //   pyenv: cannot rehash: $HOME/.pyenv/shims isn't writable
        //
        // which is emitted every time `pyenv` tries to run `rehash` (for
        // example, after installing a new Python package that drops an entry
        // point). Although the sandbox is intentionally read‑only by default,
        // writing to the user's local `pyenv` directory is safe because it
        // is already user‑writable and scoped to the current user account.
        if let Ok(home_dir) = std::env::var("HOME") {
            let pyenv_dir = PathBuf::from(home_dir).join(".pyenv");
            writable_roots.push(pyenv_dir);
        }
    }

    writable_roots.push(cwd.to_path_buf());

    writable_roots
}
