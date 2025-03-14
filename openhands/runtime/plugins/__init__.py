# Requirements
from hanzo.runtime.plugins.agent_skills import (
    AgentSkillsPlugin,
    AgentSkillsRequirement,
)
from hanzo.runtime.plugins.jupyter import JupyterPlugin, JupyterRequirement
from hanzo.runtime.plugins.requirement import Plugin, PluginRequirement
from hanzo.runtime.plugins.vscode import VSCodePlugin, VSCodeRequirement

__all__ = [
    'Plugin',
    'PluginRequirement',
    'AgentSkillsRequirement',
    'AgentSkillsPlugin',
    'JupyterRequirement',
    'JupyterPlugin',
    'VSCodeRequirement',
    'VSCodePlugin',
]

ALL_PLUGINS = {
    'jupyter': JupyterPlugin,
    'agent_skills': AgentSkillsPlugin,
    'vscode': VSCodePlugin,
}
