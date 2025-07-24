"""Tests for the setup script."""

from unittest.mock import patch

from conftest import (
    _load_runtime,
)

from ide.core.setup import initialize_repository_for_runtime
from ide.events.action import FileReadAction, FileWriteAction
from ide.events.observation import FileReadObservation, FileWriteObservation
from ide.integrations.service_types import ProviderType, Repository


def test_initialize_repository_for_runtime(temp_dir, runtime_cls, run_as_ide):
    """Test that the initialize_repository_for_runtime function works."""
    runtime, config = _load_runtime(temp_dir, runtime_cls, run_as_ide)
    mock_repo = Repository(
        id='1232',
        full_name='hanzoai/ide',
        git_provider=ProviderType.GITHUB,
        is_public=True,
    )

    with patch(
        'ide.runtime.base.ProviderHandler.verify_repo_provider',
        return_value=mock_repo,
    ):
        repository_dir = initialize_repository_for_runtime(
            runtime, selected_repository='hanzoai/ide'
        )

    assert repository_dir is not None
    assert repository_dir == 'IDE'


def test_maybe_run_setup_script(temp_dir, runtime_cls, run_as_ide):
    """Test that setup script is executed when it exists."""
    runtime, config = _load_runtime(temp_dir, runtime_cls, run_as_ide)

    setup_script = '.ide/setup.sh'
    write_obs = runtime.write(
        FileWriteAction(
            path=setup_script, content="#!/bin/bash\necho 'Hello World' >> README.md\n"
        )
    )
    assert isinstance(write_obs, FileWriteObservation)

    # Run setup script
    runtime.maybe_run_setup_script()

    # Verify script was executed by checking output
    read_obs = runtime.read(FileReadAction(path='README.md'))
    assert isinstance(read_obs, FileReadObservation)
    assert read_obs.content == 'Hello World\n'


def test_maybe_run_setup_script_with_long_timeout(temp_dir, runtime_cls, run_as_ide):
    """Test that setup script is executed when it exists."""
    runtime, config = _load_runtime(
        temp_dir,
        runtime_cls,
        run_as_ide,
        runtime_startup_env_vars={'NO_CHANGE_TIMEOUT_SECONDS': '1'},
    )

    setup_script = '.ide/setup.sh'
    write_obs = runtime.write(
        FileWriteAction(
            path=setup_script,
            content="#!/bin/bash\nsleep 3 && echo 'Hello World' >> README.md\n",
        )
    )
    assert isinstance(write_obs, FileWriteObservation)

    # Run setup script
    runtime.maybe_run_setup_script()

    # Verify script was executed by checking output
    read_obs = runtime.read(FileReadAction(path='README.md'))
    assert isinstance(read_obs, FileReadObservation)
    assert read_obs.content == 'Hello World\n'
