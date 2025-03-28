from __future__ import annotations

from dev.core.config.condenser_config import BrowserOutputCondenserConfig
from dev.events.event import Event
from dev.events.observation import BrowserOutputObservation
from dev.events.observation.agent import AgentCondensationObservation
from dev.memory.condenser.condenser import Condensation, Condenser, View


class BrowserOutputCondenser(Condenser):
    """A condenser that masks the observations from browser outputs outside of a recent attention window.

    The intent here is to mask just the browser outputs and leave everything else untouched. This is important because currently we provide screenshots and accessibility trees as input to the model for browser observations. These are really large and consume a lot of tokens without any benefits in performance. So we want to mask all such observations from all previous timesteps, and leave only the most recent one in context.
    """

    def __init__(self, attention_window: int = 1):
        self.attention_window = attention_window
        super().__init__()

    def condense(self, events: list[Event]) -> View | Condensation:
        """Replace the content of browser observations outside of the attention window with a placeholder."""
        results: list[Event] = []
        cnt: int = 0
        for event in reversed(events):
            if (
                isinstance(event, BrowserOutputObservation)
                and cnt >= self.attention_window
            ):
                results.append(
                    AgentCondensationObservation(
                        f'Current URL: {event.url}\nContent Omitted'
                    )
                )
            else:
                results.append(event)
                if isinstance(event, BrowserOutputObservation):
                    cnt += 1

        return View(events=list(reversed(results)))

    @classmethod
    def from_config(
        cls, config: BrowserOutputCondenserConfig
    ) -> BrowserOutputCondenser:
        return BrowserOutputCondenser(**config.model_dump(exclude=['type']))


BrowserOutputCondenser.register_config(BrowserOutputCondenserConfig)
