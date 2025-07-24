import os

import socketio
from dotenv import load_dotenv

from ide.core.config import load_ide_config
from ide.core.config.ide_config import IDEConfig
from ide.server.config.server_config import ServerConfig, load_server_config
from ide.server.conversation_manager.conversation_manager import (
    ConversationManager,
)
from ide.server.monitoring import MonitoringListener
from ide.server.types import ServerConfigInterface
from ide.storage import get_file_store
from ide.storage.conversation.conversation_store import ConversationStore
from ide.storage.files import FileStore
from ide.storage.secrets.secrets_store import SecretsStore
from ide.storage.settings.settings_store import SettingsStore
from ide.utils.import_utils import get_impl

load_dotenv()

config: IDEConfig = load_ide_config()
server_config_interface: ServerConfigInterface = load_server_config()
assert isinstance(server_config_interface, ServerConfig), (
    'Loaded server config interface is not a ServerConfig, despite this being assumed'
)
server_config: ServerConfig = server_config_interface
file_store: FileStore = get_file_store(
    config.file_store,
    config.file_store_path,
    config.file_store_web_hook_url,
    config.file_store_web_hook_headers,
)

client_manager = None
redis_host = os.environ.get('REDIS_HOST')
if redis_host:
    client_manager = socketio.AsyncRedisManager(
        f'redis://{redis_host}',
        redis_options={'password': os.environ.get('REDIS_PASSWORD')},
    )


sio = socketio.AsyncServer(
    async_mode='asgi',
    cors_allowed_origins='*',
    client_manager=client_manager,
    # Increase buffer size to 4MB (to handle 3MB files with base64 overhead)
    max_http_buffer_size=4 * 1024 * 1024,
)

MonitoringListenerImpl = get_impl(
    MonitoringListener,
    server_config.monitoring_listener_class,
)

monitoring_listener = MonitoringListenerImpl.get_instance(config)

ConversationManagerImpl = get_impl(
    ConversationManager,
    server_config.conversation_manager_class,
)

conversation_manager = ConversationManagerImpl.get_instance(
    sio, config, file_store, server_config, monitoring_listener
)

SettingsStoreImpl = get_impl(SettingsStore, server_config.settings_store_class)

SecretsStoreImpl = get_impl(SecretsStore, server_config.secret_store_class)

ConversationStoreImpl = get_impl(
    ConversationStore,
    server_config.conversation_store_class,
)
