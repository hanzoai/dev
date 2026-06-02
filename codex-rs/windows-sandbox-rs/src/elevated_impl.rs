use codex_protocol::models::PermissionProfile;
use codex_utils_absolute_path::AbsolutePathBuf;
use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

pub struct ElevatedSandboxProfileCaptureRequest<'a> {
    pub permission_profile: &'a PermissionProfile,
    pub workspace_roots: &'a [AbsolutePathBuf],
    pub codex_home: &'a Path,
    pub command: Vec<String>,
    pub cwd: &'a Path,
    pub env_map: HashMap<String, String>,
    pub timeout_ms: Option<u64>,
    pub cancellation: Option<crate::WindowsSandboxCancellationToken>,
    pub use_private_desktop: bool,
    pub proxy_enforced: bool,
    pub read_roots_override: Option<&'a [PathBuf]>,
    pub read_roots_include_platform_defaults: bool,
    pub write_roots_override: Option<&'a [PathBuf]>,
    pub deny_read_paths_override: &'a [AbsolutePathBuf],
    pub deny_write_paths_override: &'a [AbsolutePathBuf],
}

mod windows_impl {
    use super::ElevatedSandboxProfileCaptureRequest;
    use crate::acl::allow_null_device;
    use crate::cap::load_or_create_cap_sids;
    use crate::cap::workspace_write_cap_sid_for_root;
    use crate::env::ensure_non_interactive_pager;
    use crate::env::inherit_path_env;
    use crate::env::normalize_null_device_env;
    use crate::identity::require_logon_sandbox_creds;
    use crate::ipc_framed::EmptyPayload;
    use crate::ipc_framed::FramedMessage;
    use crate::ipc_framed::Message;
    use crate::ipc_framed::OutputStream;
    use crate::ipc_framed::SpawnRequest;
    use crate::ipc_framed::decode_bytes;
    use crate::ipc_framed::read_frame;
    use crate::ipc_framed::write_frame;
    use crate::logging::log_failure;
    use crate::logging::log_start;
    use crate::logging::log_success;
    use crate::resolved_permissions::ResolvedWindowsSandboxPermissions;
    use crate::runner_client::spawn_runner_transport;
    use crate::sandbox_utils::ensure_codex_home_exists;
    use crate::sandbox_utils::inject_git_safe_directory;
    use crate::setup::effective_write_roots_for_permissions;
    use crate::token::LocalSid;
    use anyhow::Result;
    use codex_utils_absolute_path::AbsolutePathBuf;
    use std::fs::File;
    use std::path::Path;
    use std::path::PathBuf;
    use std::ptr;
    use windows_sys::Win32::Foundation::CloseHandle;
    use windows_sys::Win32::Foundation::GetLastError;
    use windows_sys::Win32::Foundation::HANDLE;
    use windows_sys::Win32::Security::Authorization::ConvertStringSecurityDescriptorToSecurityDescriptorW;
    use windows_sys::Win32::Security::PSECURITY_DESCRIPTOR;
    use windows_sys::Win32::Security::SECURITY_ATTRIBUTES;
    use windows_sys::Win32::System::Diagnostics::Debug::SetErrorMode;
    use windows_sys::Win32::System::Pipes::ConnectNamedPipe;
    use windows_sys::Win32::System::Pipes::CreateNamedPipeW;
    const PIPE_ACCESS_INBOUND: u32 = 0x0000_0001;
    const PIPE_ACCESS_OUTBOUND: u32 = 0x0000_0002;
    use windows_sys::Win32::System::Pipes::PIPE_READMODE_BYTE;
    use windows_sys::Win32::System::Pipes::PIPE_TYPE_BYTE;
    use windows_sys::Win32::System::Pipes::PIPE_WAIT;
    use windows_sys::Win32::System::Threading::CreateProcessWithLogonW;
    use windows_sys::Win32::System::Threading::LOGON_WITH_PROFILE;
    use windows_sys::Win32::System::Threading::PROCESS_INFORMATION;
    use windows_sys::Win32::System::Threading::STARTUPINFOW;

    /// Ensures the parent directory of a path exists before writing to it.
    /// Walks upward from `start` to locate the git worktree root, following gitfile redirects.
    fn find_git_root(start: &Path) -> Option<PathBuf> {
        let mut cur = dunce::canonicalize(start).ok()?;
        loop {
            let marker = cur.join(".git");
            if marker.is_dir() {
                return Some(cur);
            }
            if marker.is_file() {
                if let Ok(txt) = std::fs::read_to_string(&marker)
                    && let Some(rest) = txt.trim().strip_prefix("gitdir:")
                {
                    let gitdir = rest.trim();
                    let resolved = if Path::new(gitdir).is_absolute() {
                        PathBuf::from(gitdir)
                    } else {
                        cur.join(gitdir)
                    };
                    return resolved.parent().map(Path::to_path_buf).or(Some(cur));
                }
                return Some(cur);
            }
            let parent = cur.parent()?;
            if parent == cur {
                return None;
            }
            cur = parent.to_path_buf();
        }
    }

    /// Creates the sandbox user's Codex home directory if it does not already exist.
    fn ensure_codex_home_exists(p: &Path) -> Result<()> {
        std::fs::create_dir_all(p)?;
        Ok(())
    }

    /// Adds a git safe.directory entry to the environment when running inside a repository.
    /// git will not otherwise allow the Sandbox user to run git commands on the repo directory
    /// which is owned by the primary user.
    fn inject_git_safe_directory(
        env_map: &mut HashMap<String, String>,
        cwd: &Path,
        _logs_base_dir: Option<&Path>,
    ) {
        if let Some(git_root) = find_git_root(cwd) {
            let mut cfg_count: usize = env_map
                .get("GIT_CONFIG_COUNT")
                .and_then(|v| v.parse::<usize>().ok())
                .unwrap_or(0);
            let git_path = git_root.to_string_lossy().replace("\\\\", "/");
            env_map.insert(
                format!("GIT_CONFIG_KEY_{cfg_count}"),
                "safe.directory".to_string(),
            );
            env_map.insert(format!("GIT_CONFIG_VALUE_{cfg_count}"), git_path);
            cfg_count += 1;
            env_map.insert("GIT_CONFIG_COUNT".to_string(), cfg_count.to_string());
        }
    }

    /// Resolves the command runner path, preferring CODEX_HOME/.sandbox/bin.
    fn find_runner_exe(codex_home: &Path, log_dir: Option<&Path>) -> PathBuf {
        resolve_helper_for_launch(HelperExecutable::CommandRunner, codex_home, log_dir)
    }

    /// Generates a unique named-pipe path used to communicate with the runner process.
    fn pipe_name(suffix: &str) -> String {
        let mut rng = SmallRng::from_entropy();
        format!(
            r"\\.\pipe\codex-runner-{:x}-{}",
            rng.r#gen::<u128>(),
            suffix
        )
    }

    /// Creates a named pipe whose DACL only allows the sandbox user to connect.
    fn create_named_pipe(name: &str, access: u32, sandbox_sid: &str) -> io::Result<HANDLE> {
        let sddl = to_wide(format!("D:(A;;GA;;;{sandbox_sid})"));
        let mut sd: PSECURITY_DESCRIPTOR = ptr::null_mut();
        let ok = unsafe {
            ConvertStringSecurityDescriptorToSecurityDescriptorW(
                sddl.as_ptr(),
                1, // SDDL_REVISION_1
                &mut sd,
                ptr::null_mut(),
            )
        };
        if ok == 0 {
            return Err(io::Error::from_raw_os_error(unsafe {
                GetLastError() as i32
            }));
        }
        let mut sa = SECURITY_ATTRIBUTES {
            nLength: std::mem::size_of::<SECURITY_ATTRIBUTES>() as u32,
            lpSecurityDescriptor: sd,
            bInheritHandle: 0,
        };
        let wide = to_wide(name);
        let h = unsafe {
            CreateNamedPipeW(
                wide.as_ptr(),
                access,
                PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT,
                1,
                65536,
                65536,
                0,
                &mut sa as *mut SECURITY_ATTRIBUTES,
            )
        };
        if h == 0 || h == windows_sys::Win32::Foundation::INVALID_HANDLE_VALUE {
            return Err(io::Error::from_raw_os_error(unsafe {
                GetLastError() as i32
            }));
        }
        Ok(h)
    }

    /// Waits for a client connection on the named pipe, tolerating an existing connection.
    fn connect_pipe(h: HANDLE) -> io::Result<()> {
        let ok = unsafe { ConnectNamedPipe(h, ptr::null_mut()) };
        if ok == 0 {
            let err = unsafe { GetLastError() };
            const ERROR_PIPE_CONNECTED: u32 = 535;
            if err != ERROR_PIPE_CONNECTED {
                return Err(io::Error::from_raw_os_error(err as i32));
            }
        }
        Ok(())
    }

    pub use crate::windows_impl::CaptureResult;

    /// Polls for cancellation and sends the runner's terminate IPC frame when requested.
    ///
    /// The 50 ms park bounds cancellation latency without busy-waiting.
    fn spawn_cancel_writer(
        pipe_write: &File,
        cancellation: Option<crate::WindowsSandboxCancellationToken>,
    ) -> Result<Option<(std::thread::JoinHandle<()>, Arc<AtomicBool>)>> {
        let Some(cancellation) = cancellation else {
            return Ok(None);
        };
        let mut pipe_write = pipe_write.try_clone()?;
        let done = Arc::new(AtomicBool::new(false));
        let done_for_thread = Arc::clone(&done);
        let handle = std::thread::spawn(move || {
            while !done_for_thread.load(Ordering::SeqCst) {
                if cancellation.is_cancelled() {
                    let _ = write_frame(
                        &mut pipe_write,
                        &FramedMessage {
                            version: 1,
                            message: Message::Terminate {
                                payload: EmptyPayload::default(),
                            },
                        },
                    );
                    break;
                }
                std::thread::park_timeout(Duration::from_millis(50));
            }
        });
        Ok(Some((handle, done)))
    }

    /// Launches the command runner under the sandbox user and captures its output.
    #[allow(clippy::too_many_arguments)]
    pub fn run_windows_sandbox_capture_for_permission_profile(
        request: ElevatedSandboxProfileCaptureRequest<'_>,
    ) -> Result<CaptureResult> {
        let ElevatedSandboxProfileCaptureRequest {
            permission_profile,
            workspace_roots,
            codex_home,
            command,
            cwd,
            mut env_map,
            timeout_ms,
            cancellation,
            use_private_desktop,
            proxy_enforced,
            read_roots_override,
            read_roots_include_platform_defaults,
            write_roots_override,
            deny_read_paths_override,
            deny_write_paths_override,
        } = request;
        let permissions =
            ResolvedWindowsSandboxPermissions::try_from_permission_profile_for_workspace_roots(
                permission_profile,
                workspace_roots,
            )?;
        let deny_read_paths_override = deny_read_paths_override
            .iter()
            .map(AbsolutePathBuf::to_path_buf)
            .collect::<Vec<_>>();
        let deny_write_paths_override = deny_write_paths_override
            .iter()
            .map(AbsolutePathBuf::to_path_buf)
            .collect::<Vec<_>>();
        normalize_null_device_env(&mut env_map);
        ensure_non_interactive_pager(&mut env_map);
        inherit_path_env(&mut env_map);
        inject_git_safe_directory(&mut env_map, cwd);
        // Use a temp-based log dir that the sandbox user can write.
        let sandbox_base = codex_home.join(".sandbox");
        ensure_codex_home_exists(&sandbox_base)?;

        let logs_base_dir: Option<&Path> = Some(sandbox_base.as_path());
        log_start(&command, logs_base_dir);
        let sandbox_creds = require_logon_sandbox_creds(
            &permissions,
            cwd,
            &env_map,
            codex_home,
            read_roots_override,
            read_roots_include_platform_defaults,
            write_roots_override,
            &deny_read_paths_override,
            &deny_write_paths_override,
            proxy_enforced,
        )?;
        // Build capability SID for ACL grants.
        let caps = load_or_create_cap_sids(codex_home)?;
        let (psid_to_use, cap_sids) = match &policy {
            SandboxPolicy::ReadOnly { .. } => {
                #[allow(clippy::unwrap_used)]
                let psid = unsafe { convert_string_sid_to_sid(&caps.readonly).unwrap() };
                (psid, vec![caps.readonly])
            }
            SandboxPolicy::WorkspaceWrite { .. } => {
                #[allow(clippy::unwrap_used)]
                let psid = unsafe { convert_string_sid_to_sid(&caps.workspace).unwrap() };
                (
                    psid,
                    vec![
                        caps.workspace,
                        crate::cap::workspace_cap_sid_for_cwd(codex_home, cwd)?,
                    ],
                )
            }
            SandboxPolicy::DangerFullAccess | SandboxPolicy::ExternalSandbox { .. } => {
                unreachable!("DangerFullAccess handled above")
            }
        };

        let AllowDenyPaths { allow: _, deny: _ } =
            compute_allow_paths(&policy, sandbox_policy_cwd, &current_dir, &env_map);
        // Deny/allow ACEs are now applied during setup; avoid per-command churn.
        unsafe {
            allow_null_device(psid_to_use);
        }

        let pipe_in_name = pipe_name("in");
        let pipe_out_name = pipe_name("out");
        let h_pipe_in = create_named_pipe(&pipe_in_name, PIPE_ACCESS_OUTBOUND, &sandbox_sid)?;
        let h_pipe_out = create_named_pipe(&pipe_out_name, PIPE_ACCESS_INBOUND, &sandbox_sid)?;

        // Launch runner as sandbox user via CreateProcessWithLogonW.
        let runner_exe = find_runner_exe(codex_home, logs_base_dir);
        let runner_cmdline = runner_exe
            .to_str()
            .map(ToString::to_string)
            .unwrap_or_else(|| "codex-command-runner.exe".to_string());
        let runner_full_cmd = format!(
            "{} {} {}",
            quote_windows_arg(&runner_cmdline),
            quote_windows_arg(&format!("--pipe-in={pipe_in_name}")),
            quote_windows_arg(&format!("--pipe-out={pipe_out_name}"))
        );
        let mut cmdline_vec: Vec<u16> = to_wide(&runner_full_cmd);
        let exe_w: Vec<u16> = to_wide(&runner_cmdline);
        let cwd_w: Vec<u16> = to_wide(cwd);

        // Minimal CPWL launch: inherit env, no desktop override, no handle inheritance.
        let env_block: Option<Vec<u16>> = None;
        let mut si: STARTUPINFOW = unsafe { std::mem::zeroed() };
        si.cb = std::mem::size_of::<STARTUPINFOW>() as u32;
        let mut pi: PROCESS_INFORMATION = unsafe { std::mem::zeroed() };
        let user_w = to_wide(&sandbox_creds.username);
        let domain_w = to_wide(".");
        let password_w = to_wide(&sandbox_creds.password);
        // Suppress WER/UI popups from the runner process so we can collect exit codes.
        let _ = unsafe { SetErrorMode(0x0001 | 0x0002) }; // SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX

        log_note(
            &format!(
                "runner launch: exe={} cmdline={} cwd={}",
                runner_exe.display(),
                runner_full_cmd,
                cwd.display()
            ),
            logs_base_dir,
        );

        // Ensure command line buffer is mutable and includes the exe as argv[0].
        let spawn_res = unsafe {
            CreateProcessWithLogonW(
                user_w.as_ptr(),
                domain_w.as_ptr(),
                password_w.as_ptr(),
                LOGON_WITH_PROFILE,
                exe_w.as_ptr(),
                cmdline_vec.as_mut_ptr(),
                windows_sys::Win32::System::Threading::CREATE_NO_WINDOW
                    | windows_sys::Win32::System::Threading::CREATE_UNICODE_ENVIRONMENT,
                env_block
                    .as_ref()
                    .map(|b| b.as_ptr() as *const c_void)
                    .unwrap_or(ptr::null()),
                cwd_w.as_ptr(),
                &si,
                &mut pi,
            )
        };
        if spawn_res == 0 {
            let err = unsafe { GetLastError() } as i32;
            log_note(
                &format!(
                    "runner launch failed before process start: exe={} cmdline={} error={err}",
                    runner_exe.display(),
                    runner_full_cmd
                ),
                logs_base_dir,
            );
            return Err(anyhow::anyhow!("CreateProcessWithLogonW failed: {err}"));
        }

        if let Err(err) = connect_pipe(h_pipe_in) {
            unsafe {
                CloseHandle(h_pipe_in);
                CloseHandle(h_pipe_out);
                if pi.hThread != 0 {
                    CloseHandle(pi.hThread);
                }
                if pi.hProcess != 0 {
                    CloseHandle(pi.hProcess);
                }
            }
            (LocalSid::from_string(&cap_sids[0])?, cap_sids)
        } else {
            let sid = LocalSid::from_string(&caps.readonly)?;
            (sid, vec![caps.readonly])
        };

        unsafe {
            allow_null_device(sid_for_null.as_ptr());
        }

        (|| -> Result<CaptureResult> {
            let spawn_request = SpawnRequest {
                command: command.clone(),
                cwd: cwd.to_path_buf(),
                env: env_map.clone(),
                permission_profile: permission_profile.clone(),
                workspace_roots: workspace_roots.to_vec(),
                codex_home: sandbox_base.clone(),
                real_codex_home: codex_home.to_path_buf(),
                cap_sids,
                timeout_ms,
                tty: false,
                stdin_open: false,
                use_private_desktop,
            };
            let transport = spawn_runner_transport(
                codex_home,
                cwd,
                &sandbox_creds,
                logs_base_dir,
                spawn_request,
            )?;
            let (pipe_write, mut pipe_read) = transport.into_files();
            let cancel_writer = spawn_cancel_writer(&pipe_write, cancellation)?;

            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            let result = loop {
                let msg = match read_frame(&mut pipe_read) {
                    Ok(Some(msg)) => msg,
                    Ok(None) => break Err(anyhow::anyhow!("runner pipe closed before exit")),
                    Err(err) => break Err(err),
                };
                match msg.message {
                    Message::SpawnReady { .. } => {}
                    Message::Output { payload } => match decode_bytes(&payload.data_b64) {
                        Ok(bytes) => match payload.stream {
                            OutputStream::Stdout => stdout.extend_from_slice(&bytes),
                            OutputStream::Stderr => stderr.extend_from_slice(&bytes),
                        },
                        Err(err) => {
                            break Err(err);
                        }
                    },
                    Message::Exit { payload } => break Ok((payload.exit_code, payload.timed_out)),
                    Message::Error { payload } => {
                        break Err(anyhow::anyhow!("runner error: {}", payload.message));
                    }
                    other => {
                        break Err(anyhow::anyhow!(
                            "unexpected runner message during capture: {other:?}"
                        ));
                    }
                }
            };
            if let Some((cancel_handle, done)) = cancel_writer {
                done.store(true, Ordering::SeqCst);
                cancel_handle.thread().unpark();
                let _ = cancel_handle.join();
            }
            drop(pipe_write);
            let (exit_code, timed_out) = result?;

            if exit_code == 0 {
                log_success(&command, logs_base_dir);
            } else {
                log_failure(&command, &format!("exit code {exit_code}"), logs_base_dir);
            }

            Ok(CaptureResult {
                exit_code,
                stdout,
                stderr,
                timed_out,
            })
        })()
    }
}

#[cfg(target_os = "windows")]
pub use windows_impl::run_windows_sandbox_capture_for_permission_profile;

#[cfg(not(target_os = "windows"))]
mod stub {
    use super::ElevatedSandboxProfileCaptureRequest;
    use anyhow::Result;
    use anyhow::bail;

    #[derive(Debug, Default)]
    pub struct CaptureResult {
        pub exit_code: i32,
        pub stdout: Vec<u8>,
        pub stderr: Vec<u8>,
        pub timed_out: bool,
    }

    /// Stub implementation for non-Windows targets; sandboxing only works on Windows.
    #[allow(clippy::too_many_arguments)]
    pub fn run_windows_sandbox_capture_for_permission_profile(
        _request: ElevatedSandboxProfileCaptureRequest<'_>,
    ) -> Result<CaptureResult> {
        bail!("Windows sandbox is only available on Windows")
    }
}

#[cfg(not(target_os = "windows"))]
pub use stub::run_windows_sandbox_capture_for_permission_profile;
