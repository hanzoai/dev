import { describe, test, expect, beforeEach, afterEach, jest } from '@jest/globals';
import { ConfigurableAgentLoop, LLMProvider } from '../src/lib/agent-loop';
import WebSocket from 'ws';
import * as http from 'http';

// Mock WebSocket
jest.mock('ws');

describe('Browser Integration', () => {
  let agentLoop: ConfigurableAgentLoop;
  let mockWebSocketServer: http.Server;
  let mockWebSocket: any;

  beforeEach(() => {
    // Mock WebSocket connection
    mockWebSocket = {
      on: jest.fn(),
      close: jest.fn(),
      send: jest.fn()
    };

    (WebSocket as jest.MockedClass<typeof WebSocket>).mockImplementation(() => mockWebSocket);

    // Create agent loop with browser enabled
    const provider: LLMProvider = {
      name: 'Test Provider',
      type: 'local',
      model: 'test-model',
      supportsTools: true,
      supportsStreaming: false
    };

    agentLoop = new ConfigurableAgentLoop({
      provider,
      maxIterations: 10,
      enableMCP: false,
      enableBrowser: true,
      enableSwarm: false,
      streamOutput: false,
      confirmActions: false
    });
  });

  afterEach(() => {
    jest.clearAllMocks();
    if (mockWebSocketServer) {
      mockWebSocketServer.close();
    }
  });

  describe('browser tool registration', () => {
    test('should detect and connect to browser extension', async () => {
      // Simulate successful WebSocket connection
      mockWebSocket.on.mockImplementation((event: string, handler: Function) => {
        if (event === 'open') {
          setTimeout(() => handler(), 10);
        }
      });

      // Mock checkBrowserExtension to return true
      (agentLoop as any).checkBrowserExtension = jest.fn().mockResolvedValue(true);

      await agentLoop.initialize();

      // Verify browser tools were registered
      const tools = (agentLoop as any).functionCalling.getAvailableTools();
      const browserTools = tools.filter((t: any) => t.name.startsWith('browser_'));
      
      expect(browserTools).toHaveLength(4);
      expect(browserTools.map((t: any) => t.name)).toContain('browser_navigate');
      expect(browserTools.map((t: any) => t.name)).toContain('browser_click');
      expect(browserTools.map((t: any) => t.name)).toContain('browser_screenshot');
      expect(browserTools.map((t: any) => t.name)).toContain('browser_fill');
    });

    test('should fall back to Hanzo Browser if extension not available', async () => {
      // Mock extension check to fail
      (agentLoop as any).checkBrowserExtension = jest.fn().mockResolvedValue(false);
      
      // Mock browser check to succeed
      global.fetch = jest.fn().mockResolvedValue({ ok: true });
      
      await agentLoop.initialize();

      // Verify browser tools were still registered
      const tools = (agentLoop as any).functionCalling.getAvailableTools();
      const browserTools = tools.filter((t: any) => t.name.startsWith('browser_'));
      
      expect(browserTools).toHaveLength(4);
    });
  });

  describe('browser actions', () => {
    test('should navigate to URL', async () => {
      const result = await (agentLoop as any).browserNavigate('https://example.com');
      
      expect(result).toEqual({
        success: true,
        url: 'https://example.com'
      });
    });

    test('should click element', async () => {
      const result = await (agentLoop as any).browserClick('#submit-button');
      
      expect(result).toEqual({
        success: true,
        selector: '#submit-button'
      });
    });

    test('should take screenshot', async () => {
      const result = await (agentLoop as any).browserScreenshot(true);
      
      expect(result).toEqual({
        success: true,
        screenshot: 'base64_image_data'
      });
    });

    test('should fill form field', async () => {
      const result = await (agentLoop as any).browserFill('#email', 'test@example.com');
      
      expect(result).toEqual({
        success: true,
        selector: '#email',
        value: 'test@example.com'
      });
    });
  });

  describe('browser action execution via LLM', () => {
    test('should execute browser navigation through agent loop', async () => {
      // Mock LLM to return browser navigation tool call
      (agentLoop as any).callLLM = jest.fn().mockResolvedValue({
        role: 'assistant',
        content: 'I will navigate to the website.',
        toolCalls: [{
          id: 'call_1',
          name: 'browser_navigate',
          arguments: { url: 'https://example.com' }
        }]
      });

      // Mock tool execution
      (agentLoop as any).functionCalling.callFunctions = jest.fn()
        .mockResolvedValue([{ success: true, url: 'https://example.com' }]);

      await agentLoop.initialize();
      await agentLoop.execute('Navigate to example.com');

      // Verify tool was called
      expect((agentLoop as any).functionCalling.callFunctions).toHaveBeenCalledWith([{
        id: 'call_1',
        name: 'browser_navigate',
        arguments: { url: 'https://example.com' }
      }]);
    });

    test('should handle browser action errors', async () => {
      // Mock LLM to return browser action
      (agentLoop as any).callLLM = jest.fn().mockResolvedValue({
        role: 'assistant',
        content: 'I will click the button.',
        toolCalls: [{
          id: 'call_2',
          name: 'browser_click',
          arguments: { selector: '#missing-button' }
        }]
      });

      // Mock tool execution to fail
      (agentLoop as any).functionCalling.callFunctions = jest.fn()
        .mockRejectedValue(new Error('Element not found'));

      await agentLoop.initialize();

      // Execute should handle the error gracefully
      await expect(agentLoop.execute('Click the submit button')).resolves.not.toThrow();
    });
  });

  describe('browser-based evaluation scenarios', () => {
    test('should handle multi-step browser automation', async () => {
      const responses = [
        {
          role: 'assistant',
          content: 'I will navigate to the login page.',
          toolCalls: [{
            id: 'nav_1',
            name: 'browser_navigate',
            arguments: { url: 'https://example.com/login' }
          }]
        },
        {
          role: 'assistant',
          content: 'I will fill in the login form.',
          toolCalls: [
            {
              id: 'fill_1',
              name: 'browser_fill',
              arguments: { selector: '#username', value: 'testuser' }
            },
            {
              id: 'fill_2',
              name: 'browser_fill',
              arguments: { selector: '#password', value: 'testpass' }
            }
          ]
        },
        {
          role: 'assistant',
          content: 'I will submit the form.',
          toolCalls: [{
            id: 'click_1',
            name: 'browser_click',
            arguments: { selector: '#submit' }
          }]
        },
        {
          role: 'assistant',
          content: 'Login completed successfully.',
          toolCalls: []
        }
      ];

      let callCount = 0;
      (agentLoop as any).callLLM = jest.fn().mockImplementation(() => {
        return Promise.resolve(responses[callCount++]);
      });

      (agentLoop as any).functionCalling.callFunctions = jest.fn()
        .mockResolvedValue([{ success: true }]);

      await agentLoop.initialize();
      await agentLoop.execute('Login to the website with username "testuser"');

      // Verify all browser actions were executed
      expect((agentLoop as any).functionCalling.callFunctions).toHaveBeenCalledTimes(3);
    });
  });
});