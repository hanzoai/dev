import os

from hanzo.utils.import_utils import get_impl


class ConversationValidator:
    """Storage for conversation metadata. May or may not support multiple users depending on the environment."""

    async def validate(self, conversation_id: str, cookies_str: str):
        return None


conversation_validator_cls = os.environ.get(
    'HANZO_CONVERSATION_VALIDATOR_CLS',
    'hanzo.storage.conversation.conversation_validator.ConversationValidator',
)
ConversationValidatorImpl = get_impl(ConversationValidator, conversation_validator_cls)
