from dev.agenthub.visualbrowsing_agent.visualbrowsing_agent import (
    VisualBrowsingAgent,
)
from dev.controller.agent import Agent

Agent.register('VisualBrowsingAgent', VisualBrowsingAgent)
