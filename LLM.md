# Hanzo Dev Project Knowledge Base

## Package Configuration

### Project URLs in pyproject.toml

As of March 2025, we've fixed an issue with the package configuration in `pyproject.toml`. The repository URL was incorrectly specified in the optional dependencies section as:

```toml
[project.optional-dependencies]
urls = ["https://github.com/hanzoai/dev"]
```

This format doesn't comply with PEP 508 (Python dependency specification standard). We've moved it to the proper section according to the PyPA specification:

```toml
[project.urls]
Repository = "https://github.com/hanzoai/dev"
```

This change resolves build and installation errors related to invalid pyproject.toml configuration.

## Project Overview

Hanzo Dev (formerly OpenDevin) is a platform for software development agents powered by AI. The agents can modify code, run commands, browse the web, call APIs, and more.

## Settings Migration from localStorage to Server API

### Architectural Pattern

The project has recently migrated its settings storage approach from browser localStorage to a server-side API. This change follows the pattern:

1. **API-First Storage**: Settings are stored server-side via API endpoints
2. **React Query Integration**: Data fetching and mutations use React Query for efficient caching and synchronization
3. **Context-Based Access**: Settings are provided through a React context for component access
4. **Controlled Updates**: Changes are made through mutations with proper error handling

### Implementation Details

#### Key Files:
- `/frontend/src/services/settings.ts`: Core settings service
- `/frontend/src/hooks/query/use-settings.ts`: React Query hook for retrieving settings
- `/frontend/src/hooks/mutation/use-save-settings.ts`: React Query mutation hook for updating settings
- `/frontend/src/context/settings-context.tsx`: React context provider for settings
- `/dev/server/routes/settings.py`: Server-side API endpoints

#### Data Flow:
1. Frontend fetches settings via React Query from `/api/settings` endpoint
2. Components access settings through the `useSettings()` hook
3. Settings updates are made through `useSaveSettings()` hook
4. Server validates and stores settings
5. UI is automatically updated via React Query's cache management

### Testing Approach

Tests have been updated to use proper mocking of the API layer instead of directly manipulating localStorage. This involves:

1. Mocking the `getSettings()` and `saveSettings()` API calls
2. Providing test values through the React Query provider
3. Testing component behavior rather than implementation details
4. Focusing on application user experience flows

### Migration Work

As of March 2025, the migration has been completed with:
- All direct localStorage references for settings replaced with API calls
- Tests updated to use the API approach
- Components using the context provider for settings access

## Other Key Architecture Notes

- The application uses React for the frontend with React Query for data fetching
- Authentication is handled through a separate Auth context
- Code follows a context + hooks pattern for state management
- Tests are primarily component-based with some integration tests

## Development Guidelines

When working with settings:
1. Always use the `useSettings()` hook to access settings
2. Use the `useSaveSettings()` hook for updating settings
3. Never directly manipulate settings in localStorage
4. Keep all settings properly typed according to defined interfaces
