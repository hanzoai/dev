/**
 * ZAP Agent Example - C
 *
 * Demonstrates a ZAP agent with tool execution in C.
 * Uses libzap FFI bindings (to be implemented).
 *
 * Compile:
 *   gcc -o agent agent.c -lzap
 *
 * Run:
 *   ./agent
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <sys/stat.h>

/* ZAP types (from libzap.h - to be generated from Rust) */
typedef enum {
    APPROVAL_UNTRUSTED,
    APPROVAL_ON_FAILURE,
    APPROVAL_ON_REQUEST,
    APPROVAL_NEVER
} AskForApproval;

typedef enum {
    SANDBOX_DANGER_FULL_ACCESS,
    SANDBOX_READ_ONLY,
    SANDBOX_WORKSPACE_WRITE
} SandboxMode;

typedef struct {
    char* cwd;
    char* session_id;
    AskForApproval approval_policy;
    SandboxMode sandbox_mode;
    int timeout_ms;
} ExecutorContext;

typedef struct {
    char* content;
    char* error;
    int content_len;
} ToolResult;

/* Tool implementations */
ToolResult read_file(ExecutorContext* ctx, const char* path) {
    ToolResult result = {NULL, NULL, 0};
    char full_path[1024];
    snprintf(full_path, sizeof(full_path), "%s/%s", ctx->cwd, path);

    FILE* f = fopen(full_path, "r");
    if (!f) {
        result.error = strdup("Failed to open file");
        return result;
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    result.content = malloc(size + 1);
    if (result.content) {
        fread(result.content, 1, size, f);
        result.content[size] = '\0';
        result.content_len = size;
    }
    fclose(f);
    return result;
}

ToolResult list_dir(ExecutorContext* ctx, const char* path) {
    ToolResult result = {NULL, NULL, 0};
    char full_path[1024];
    snprintf(full_path, sizeof(full_path), "%s/%s", ctx->cwd, path);

    DIR* dir = opendir(full_path);
    if (!dir) {
        result.error = strdup("Failed to open directory");
        return result;
    }

    /* Build JSON array */
    char buffer[8192] = "[";
    struct dirent* entry;
    int first = 1;

    while ((entry = readdir(dir)) != NULL) {
        if (entry->d_name[0] == '.') continue; /* Skip hidden */

        char entry_json[256];
        snprintf(entry_json, sizeof(entry_json),
            "%s{\"name\":\"%s\",\"isDir\":%s}",
            first ? "" : ",",
            entry->d_name,
            entry->d_type == DT_DIR ? "true" : "false"
        );
        strncat(buffer, entry_json, sizeof(buffer) - strlen(buffer) - 1);
        first = 0;
    }
    strncat(buffer, "]", sizeof(buffer) - strlen(buffer) - 1);
    closedir(dir);

    result.content = strdup(buffer);
    result.content_len = strlen(buffer);
    return result;
}

ToolResult git_status(ExecutorContext* ctx) {
    ToolResult result = {NULL, NULL, 0};
    char cmd[1024];
    snprintf(cmd, sizeof(cmd), "cd %s && git status --porcelain=v2 --branch 2>&1", ctx->cwd);

    FILE* pipe = popen(cmd, "r");
    if (!pipe) {
        result.error = strdup("Failed to run git status");
        return result;
    }

    char buffer[4096] = "";
    char line[256];
    while (fgets(line, sizeof(line), pipe)) {
        strncat(buffer, line, sizeof(buffer) - strlen(buffer) - 1);
    }
    pclose(pipe);

    result.content = strdup(buffer);
    result.content_len = strlen(buffer);
    return result;
}

void free_result(ToolResult* result) {
    if (result->content) free(result->content);
    if (result->error) free(result->error);
}

void print_tools() {
    const char* tools[] = {
        "read_file",
        "list_dir",
        "git_status",
        "git_log",
        "exec"
    };
    int count = sizeof(tools) / sizeof(tools[0]);

    printf("Available tools (%d):\n", count);
    for (int i = 0; i < count; i++) {
        printf("  - %s\n", tools[i]);
    }
}

int main(int argc, char** argv) {
    printf("ZAP C Agent Example\n");
    printf("===================\n\n");

    /* Initialize context */
    ExecutorContext ctx = {
        .cwd = ".",
        .session_id = "c-agent-001",
        .approval_policy = APPROVAL_ON_REQUEST,
        .sandbox_mode = SANDBOX_WORKSPACE_WRITE,
        .timeout_ms = 30000
    };

    /* List tools */
    print_tools();
    printf("\n");

    /* Example 1: Read file */
    printf("Example 1: Read file\n");
    ToolResult result = read_file(&ctx, "Cargo.toml");
    if (result.error) {
        printf("  Error: %s\n", result.error);
    } else {
        printf("  Result: %d bytes read\n", result.content_len);
    }
    free_result(&result);
    printf("\n");

    /* Example 2: List directory */
    printf("Example 2: List directory\n");
    result = list_dir(&ctx, ".");
    if (result.error) {
        printf("  Error: %s\n", result.error);
    } else {
        char preview[101];
        strncpy(preview, result.content, 100);
        preview[100] = '\0';
        printf("  Result: %s...\n", preview);
    }
    free_result(&result);
    printf("\n");

    /* Example 3: Git status */
    printf("Example 3: Git status\n");
    result = git_status(&ctx);
    if (result.error) {
        printf("  Error: %s\n", result.error);
    } else {
        char preview[101];
        strncpy(preview, result.content, 100);
        preview[100] = '\0';
        printf("  Result: %s...\n", preview);
    }
    free_result(&result);

    return 0;
}
