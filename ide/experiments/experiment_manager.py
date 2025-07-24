import os

from ide.server.session.conversation_init_data import ConversationInitData
from ide.utils.import_utils import get_impl


class ExperimentManager:
    @staticmethod
    def run_conversation_variant_test(
        user_id: str, conversation_id: str, conversation_settings: ConversationInitData
    ) -> ConversationInitData:
        return conversation_settings


experiment_manager_cls = os.environ.get(
    'IDE_EXPERIMENT_MANAGER_CLS',
    'ide.experiments.experiment_manager.ExperimentManager',
)
ExperimentManagerImpl = get_impl(ExperimentManager, experiment_manager_cls)
