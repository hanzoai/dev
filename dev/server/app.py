import warnings
from contextlib import asynccontextmanager

with warnings.catch_warnings():
    warnings.simplefilter('ignore')

from fastapi import (
    FastAPI,
)

import dev.agenthub  # noqa F401 (we import this to get the agents registered)
from dev import __version__
from dev.server.routes.conversation import app as conversation_api_router
from dev.server.routes.feedback import app as feedback_api_router
from dev.server.routes.files import app as files_api_router
from dev.server.routes.github import app as github_api_router
from dev.server.routes.manage_conversations import (
    app as manage_conversation_api_router,
)
from dev.server.routes.public import app as public_api_router
from dev.server.routes.security import app as security_api_router
from dev.server.routes.settings import app as settings_router
from dev.server.routes.trajectory import app as trajectory_router
from dev.server.shared import conversation_manager


@asynccontextmanager
async def _lifespan(app: FastAPI):
    async with conversation_manager:
        yield


app = FastAPI(
    title='Dev',
    description='Dev: Code Less, Make More',
    version=__version__,
    lifespan=_lifespan,
)


@app.get('/health')
async def health():
    return 'OK'


app.include_router(public_api_router)
app.include_router(files_api_router)
app.include_router(security_api_router)
app.include_router(feedback_api_router)
app.include_router(conversation_api_router)
app.include_router(manage_conversation_api_router)
app.include_router(settings_router)
app.include_router(github_api_router)
app.include_router(trajectory_router)
