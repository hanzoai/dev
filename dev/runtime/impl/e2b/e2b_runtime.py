from typing import Callable

from dev.core.config import AppConfig
from dev.events.action import (
    FileReadAction,
    FileWriteAction,
)
from dev.events.observation import (
    ErrorObservation,
    FileReadObservation,
    FileWriteObservation,
    Observation,
)
from dev.events.stream import EventStream
from dev.runtime.base import Runtime
from dev.runtime.impl.e2b.filestore import E2BFileStore
from dev.runtime.impl.e2b.sandbox import E2BSandbox
from dev.runtime.plugins import PluginRequirement
from dev.runtime.utils.files import insert_lines, read_lines


class E2BRuntime(Runtime):
    def __init__(
        self,
        config: AppConfig,
        event_stream: EventStream,
        sid: str = 'default',
        plugins: list[PluginRequirement] | None = None,
        sandbox: E2BSandbox | None = None,
        status_callback: Callable | None = None,
    ):
        super().__init__(
            config,
            event_stream,
            sid,
            plugins,
            status_callback=status_callback,
        )
        if sandbox is None:
            self.sandbox = E2BSandbox()
        if not isinstance(self.sandbox, E2BSandbox):
            raise ValueError('E2BRuntime requires an E2BSandbox')
        self.file_store = E2BFileStore(self.sandbox.filesystem)

    def read(self, action: FileReadAction) -> Observation:
        content = self.file_store.read(action.path)
        lines = read_lines(content.split('\n'), action.start, action.end)
        code_view = ''.join(lines)
        return FileReadObservation(code_view, path=action.path)

    def write(self, action: FileWriteAction) -> Observation:
        if action.start == 0 and action.end == -1:
            self.file_store.write(action.path, action.content)
            return FileWriteObservation(content='', path=action.path)
        files = self.file_store.list(action.path)
        if action.path in files:
            all_lines = self.file_store.read(action.path).split('\n')
            new_file = insert_lines(
                action.content.split('\n'), all_lines, action.start, action.end
            )
            self.file_store.write(action.path, ''.join(new_file))
            return FileWriteObservation('', path=action.path)
        else:
            # FIXME: we should create a new file here
            return ErrorObservation(f'File not found: {action.path}')
