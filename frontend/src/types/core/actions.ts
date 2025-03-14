import { HanzoActionEvent } from "./base";
import { ActionSecurityRisk } from "#/state/security-analyzer-slice";

export interface UserMessageAction extends HanzoActionEvent<"message"> {
  source: "user";
  args: {
    content: string;
    image_urls: string[];
  };
}

export interface CommandAction extends HanzoActionEvent<"run"> {
  source: "agent";
  args: {
    command: string;
    security_risk: ActionSecurityRisk;
    confirmation_state: "confirmed" | "rejected" | "awaiting_confirmation";
    thought: string;
    hidden?: boolean;
  };
}

export interface AssistantMessageAction
  extends HanzoActionEvent<"message"> {
  source: "agent";
  args: {
    thought: string;
    image_urls: string[] | null;
    wait_for_response: boolean;
  };
}

export interface IPythonAction extends HanzoActionEvent<"run_ipython"> {
  source: "agent";
  args: {
    code: string;
    security_risk: ActionSecurityRisk;
    confirmation_state: "confirmed" | "rejected" | "awaiting_confirmation";
    kernel_init_code: string;
    thought: string;
  };
}

export interface ThinkAction extends HanzoActionEvent<"think"> {
  source: "agent";
  args: {
    thought: string;
  };
}

export interface FinishAction extends HanzoActionEvent<"finish"> {
  source: "agent";
  args: {
    final_thought: string;
    task_completed: "success" | "failure" | "partial";
    outputs: Record<string, unknown>;
    thought: string;
  };
}

export interface DelegateAction extends HanzoActionEvent<"delegate"> {
  source: "agent";
  timeout: number;
  args: {
    agent: "BrowsingAgent";
    inputs: Record<string, string>;
    thought: string;
  };
}

export interface BrowseAction extends HanzoActionEvent<"browse"> {
  source: "agent";
  args: {
    url: string;
    thought: string;
  };
}

export interface BrowseInteractiveAction
  extends HanzoActionEvent<"browse_interactive"> {
  source: "agent";
  timeout: number;
  args: {
    browser_actions: string;
    thought: string | null;
    browsergym_send_msg_to_user: string;
  };
}

export interface FileReadAction extends HanzoActionEvent<"read"> {
  source: "agent";
  args: {
    path: string;
    thought: string;
    security_risk: ActionSecurityRisk | null;
    impl_source?: string;
    view_range?: number[] | null;
  };
}

export interface FileWriteAction extends HanzoActionEvent<"write"> {
  source: "agent";
  args: {
    path: string;
    content: string;
    thought: string;
  };
}

export interface FileEditAction extends HanzoActionEvent<"edit"> {
  source: "agent";
  args: {
    path: string;
    command?: string;
    file_text?: string | null;
    view_range?: number[] | null;
    old_str?: string | null;
    new_str?: string | null;
    insert_line?: number | null;
    content?: string;
    start?: number;
    end?: number;
    thought: string;
    security_risk: ActionSecurityRisk | null;
    impl_source?: string;
  };
}

export interface RejectAction extends HanzoActionEvent<"reject"> {
  source: "agent";
  args: {
    thought: string;
  };
}

export type HanzoAction =
  | UserMessageAction
  | AssistantMessageAction
  | CommandAction
  | IPythonAction
  | ThinkAction
  | FinishAction
  | DelegateAction
  | BrowseAction
  | BrowseInteractiveAction
  | FileReadAction
  | FileEditAction
  | FileWriteAction
  | RejectAction;
