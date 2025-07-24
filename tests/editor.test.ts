import { describe, test, expect, beforeEach, afterEach } from 'vitest';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { FileEditor, EditCommand } from '../src/lib/editor';

describe('Editor', () => {
  let editor: FileEditor;
  let testDir: string;
  let testFile: string;

  beforeEach(() => {
    // Create temporary test directory
    testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'hanzo-dev-test-'));
    testFile = path.join(testDir, 'test.txt');
    editor = new FileEditor(testDir);
  });

  afterEach(() => {
    // Clean up test directory
    fs.rmSync(testDir, { recursive: true, force: true });
  });

  describe('create command', () => {
    test('should create a new file with content', async () => {
      const command: EditCommand = {
        command: 'create',
        path: testFile,
        content: 'Hello, World!'
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(true);
      expect(fs.existsSync(testFile)).toBe(true);
      expect(fs.readFileSync(testFile, 'utf-8')).toBe('Hello, World!');
    });

    test('should fail when file already exists', async () => {
      fs.writeFileSync(testFile, 'existing content');

      const command: EditCommand = {
        command: 'create',
        path: testFile,
        content: 'new content'
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(false);
      expect(result.error).toBe('FILE_EXISTS');
    });
  });

  describe('view command', () => {
    test('should view entire file when no line range specified', async () => {
      const content = 'Line 1\nLine 2\nLine 3';
      fs.writeFileSync(testFile, content);

      const command: EditCommand = {
        command: 'view',
        path: testFile
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(true);
      expect(result.content).toContain('Line 1');
      expect(result.content).toContain('Line 2');
      expect(result.content).toContain('Line 3');
    });

    test('should view specific line range', async () => {
      const lines = Array.from({ length: 10 }, (_, i) => `Line ${i + 1}`);
      fs.writeFileSync(testFile, lines.join('\n'));

      const command: EditCommand = {
        command: 'view',
        path: testFile,
        startLine: 3,
        endLine: 5
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(true);
      expect(result.content).toContain('Line 3');
      expect(result.content).toContain('Line 4');
      expect(result.content).toContain('Line 5');
      expect(result.content).not.toContain('Line 1');
      expect(result.content).not.toContain('Line 10');
    });
  });

  describe('str_replace command', () => {
    test('should replace string in file', async () => {
      const content = 'Hello, World!\nThis is a test.\nHello again!';
      fs.writeFileSync(testFile, content);

      const command: EditCommand = {
        command: 'str_replace',
        path: testFile,
        oldStr: 'Hello, World!',
        newStr: 'Hi, Universe!'
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(true);
      
      const newContent = fs.readFileSync(testFile, 'utf-8');
      expect(newContent).toContain('Hi, Universe!');
      expect(newContent).not.toContain('Hello, World!');
      expect(newContent).toContain('Hello again!');
    });

    test('should fail when old string not found', async () => {
      fs.writeFileSync(testFile, 'Some content');

      const command: EditCommand = {
        command: 'str_replace',
        path: testFile,
        oldStr: 'Not found',
        newStr: 'Replacement'
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(false);
      expect(result.error).toBe('STRING_NOT_FOUND');
    });

    test('should fail when old string appears multiple times', async () => {
      const content = 'duplicate\nsome text\nduplicate';
      fs.writeFileSync(testFile, content);

      const command: EditCommand = {
        command: 'str_replace',
        path: testFile,
        oldStr: 'duplicate',
        newStr: 'unique'
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(false);
      expect(result.error).toBe('STRING_NOT_UNIQUE');
    });
  });

  describe('insert command', () => {
    test('should insert text at specific line', async () => {
      const content = 'Line 1\nLine 2\nLine 3';
      fs.writeFileSync(testFile, content);

      const command: EditCommand = {
        command: 'insert',
        path: testFile,
        lineNumber: 2,
        content: 'Inserted line'
      };

      const result = await editor.execute(command);
      expect(result.success).toBe(true);

      const newContent = fs.readFileSync(testFile, 'utf-8');
      const lines = newContent.split('\n');
      expect(lines[1]).toBe('Inserted line');
      expect(lines[2]).toBe('Line 2');
    });
  });

  describe('undo_edit command', () => {
    test('should undo last edit', async () => {
      const originalContent = 'Original content';
      fs.writeFileSync(testFile, originalContent);

      // Make an edit
      await editor.execute({
        command: 'str_replace',
        path: testFile,
        oldStr: 'Original',
        newStr: 'Modified'
      });

      // Verify edit was made
      expect(fs.readFileSync(testFile, 'utf-8')).toContain('Modified');

      // Undo the edit
      const result = await editor.execute({
        command: 'undo_edit',
        path: testFile
      });

      expect(result.success).toBe(true);
      expect(fs.readFileSync(testFile, 'utf-8')).toBe(originalContent);
    });
  });

  describe('chunk localization', () => {
    test('should find relevant chunks for search query', async () => {
      const codeContent = `
function calculateTotal(items) {
  let total = 0;
  for (const item of items) {
    total += item.price * item.quantity;
  }
  return total;
}

function applyDiscount(total, discountPercent) {
  return total * (1 - discountPercent / 100);
}

function formatCurrency(amount) {
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(amount);
}
`;
      fs.writeFileSync(testFile, codeContent);

      const chunks = await editor.getRelevantChunks(testFile, 'calculate price total');
      expect(chunks.length).toBeGreaterThan(0);
      expect(chunks[0].content).toContain('calculateTotal');
      expect(chunks[0].content).toContain('price');
    });
  });
});