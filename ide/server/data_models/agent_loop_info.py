from dataclasses import dataclass, field

from ide.events.event_store_abc import EventStoreABC
from ide.runtime.runtime_status import RuntimeStatus
from ide.storage.data_models.conversation_status import ConversationStatus


@dataclass
class AgentLoopInfo:
    """
    Information about an agent loop - the URL on which to locate it and the event store
    """

    conversation_id: str
    url: str | None
    session_api_key: str | None
    event_store: EventStoreABC | None
    status: ConversationStatus = field(default=ConversationStatus.RUNNING)
    runtime_status: RuntimeStatus | None = None
