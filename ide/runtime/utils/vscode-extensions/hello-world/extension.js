const vscode = require('vscode');

function activate(context) {
    let disposable = vscode.commands.registerCommand('ide-hello-world.helloWorld', function () {
        vscode.window.showInformationMessage('Hello from IDE!');
    });

    context.subscriptions.push(disposable);
}

function deactivate() {}

module.exports = {
    activate,
    deactivate
}
