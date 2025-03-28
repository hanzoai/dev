const vscode = require('vscode');

function activate(context) {
    let disposable = vscode.commands.registerCommand('dev-hello-world.helloWorld', function () {
        vscode.window.showInformationMessage('Hello from Dev!');
    });

    context.subscriptions.push(disposable);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
}
