import * as fs from 'fs';
import * as path from 'path';
import chalk from 'chalk';

export interface EditCommand {
  command: 'view' | 'create' | 'str_replace' | 'insert' | 'undo_edit';
  path?: string;
  content?: string;
  oldStr?: string;
  newStr?: string;
  startLine?: number;
  endLine?: number;
  lineNumber?: number;
}

export interface EditResult {
  success: boolean;
  message: string;
  content?: string;
  error?: string;
}

export class FileEditor {
  private static readonly MAX_LINES_TO_EDIT = 300;
  private static readonly SUPPORTED_BINARY_FORMATS = [
    '.pdf', '.docx', '.xlsx', '.mp3', '.mp4', '.jpg', '.jpeg', '.png', '.gif'
  ];
  
  private editHistory: Map<string, string[]> = new Map();
  private currentFile: string | null = null;

  constructor(workingDir: string = process.cwd()) {
    // No need to instantiate ChunkLocalizer as it has static methods
  }

  async execute(command: EditCommand): Promise<EditResult> {
    switch (command.command) {
      case 'view':
        return this.viewFile(command.path!, command.startLine, command.endLine);
      case 'create':
        return this.createFile(command.path!, command.content || '');
      case 'str_replace':
        return this.strReplace(command.path!, command.oldStr!, command.newStr!);
      case 'insert':
        return this.insertLine(command.path!, command.lineNumber!, command.content!);
      case 'undo_edit':
        return this.undoEdit(command.path!);
      default:
        return {
          success: false,
          message: `Unknown command: ${command.command}`,
          error: 'Invalid command'
        };
    }
  }

  private async viewFile(filePath: string, startLine?: number, endLine?: number): Promise<EditResult> {
    try {
      if (!fs.existsSync(filePath)) {
        return {
          success: false,
          message: `File not found: ${filePath}`,
          error: 'FILE_NOT_FOUND'
        };
      }

      const ext = path.extname(filePath).toLowerCase();
      if (FileEditor.SUPPORTED_BINARY_FORMATS.includes(ext)) {
        const stats = fs.statSync(filePath);
        return {
          success: true,
          message: `Binary file: ${filePath} (${this.formatBytes(stats.size)})`,
          content: `[Binary file of type ${ext}]`
        };
      }

      const content = fs.readFileSync(filePath, 'utf-8');
      const lines = content.split('\n');
      
      if (startLine !== undefined || endLine !== undefined) {
        const start = (startLine || 1) - 1;
        const end = endLine || lines.length;
        const viewLines = lines.slice(start, end);
        
        const result = viewLines.map((line, idx) => 
          `${chalk.gray(`${start + idx + 1}:`)} ${line}`
        ).join('\n');

        return {
          success: true,
          message: `Viewing ${filePath} lines ${start + 1}-${end}`,
          content: result
        };
      }

      // Full file view with line numbers
      const result = lines.map((line, idx) => 
        `${chalk.gray(`${idx + 1}:`)} ${line}`
      ).join('\n');

      this.currentFile = filePath;
      
      return {
        success: true,
        message: `Viewing ${filePath} (${lines.length} lines)`,
        content: result
      };
    } catch (error) {
      return {
        success: false,
        message: `Error reading file: ${error}`,
        error: 'READ_ERROR'
      };
    }
  }

  private async createFile(filePath: string, content: string): Promise<EditResult> {
    try {
      if (fs.existsSync(filePath)) {
        return {
          success: false,
          message: `File already exists: ${filePath}`,
          error: 'FILE_EXISTS'
        };
      }

      const dir = path.dirname(filePath);
      if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir, { recursive: true });
      }

      fs.writeFileSync(filePath, content);
      this.addToHistory(filePath, '');

      return {
        success: true,
        message: `Created file: ${filePath}`,
        content: content
      };
    } catch (error) {
      return {
        success: false,
        message: `Error creating file: ${error}`,
        error: 'CREATE_ERROR'
      };
    }
  }

  private async strReplace(filePath: string, oldStr: string, newStr: string): Promise<EditResult> {
    try {
      if (!fs.existsSync(filePath)) {
        return {
          success: false,
          message: `File not found: ${filePath}`,
          error: 'FILE_NOT_FOUND'
        };
      }

      const content = fs.readFileSync(filePath, 'utf-8');
      
      // Check if old string exists
      if (!content.includes(oldStr)) {
        return {
          success: false,
          message: `String not found in file: "${oldStr}"`,
          error: 'STRING_NOT_FOUND'
        };
      }

      // Check if old string is unique
      const occurrences = content.split(oldStr).length - 1;
      if (occurrences > 1) {
        return {
          success: false,
          message: `String "${oldStr}" found ${occurrences} times. Please provide a unique string.`,
          error: 'STRING_NOT_UNIQUE'
        };
      }

      // Save to history
      this.addToHistory(filePath, content);

      // Replace
      const newContent = content.replace(oldStr, newStr);
      fs.writeFileSync(filePath, newContent);

      return {
        success: true,
        message: `Replaced string in ${filePath}`,
        content: this.showDiff(oldStr, newStr)
      };
    } catch (error) {
      return {
        success: false,
        message: `Error replacing string: ${error}`,
        error: 'REPLACE_ERROR'
      };
    }
  }

  private async insertLine(filePath: string, lineNumber: number, content: string): Promise<EditResult> {
    try {
      if (!fs.existsSync(filePath)) {
        return {
          success: false,
          message: `File not found: ${filePath}`,
          error: 'FILE_NOT_FOUND'
        };
      }

      const fileContent = fs.readFileSync(filePath, 'utf-8');
      const lines = fileContent.split('\n');

      if (lineNumber < 1 || lineNumber > lines.length + 1) {
        return {
          success: false,
          message: `Invalid line number: ${lineNumber}. File has ${lines.length} lines.`,
          error: 'INVALID_LINE_NUMBER'
        };
      }

      // Save to history
      this.addToHistory(filePath, fileContent);

      // Insert line
      lines.splice(lineNumber - 1, 0, content);
      const newContent = lines.join('\n');
      fs.writeFileSync(filePath, newContent);

      return {
        success: true,
        message: `Inserted line at ${lineNumber} in ${filePath}`,
        content: content
      };
    } catch (error) {
      return {
        success: false,
        message: `Error inserting line: ${error}`,
        error: 'INSERT_ERROR'
      };
    }
  }

  private async undoEdit(filePath: string): Promise<EditResult> {
    const history = this.editHistory.get(filePath);
    if (!history || history.length === 0) {
      return {
        success: false,
        message: `No edit history for ${filePath}`,
        error: 'NO_HISTORY'
      };
    }

    const previousContent = history.pop()!;
    fs.writeFileSync(filePath, previousContent);

    return {
      success: true,
      message: `Undid last edit to ${filePath}`,
      content: 'Edit undone'
    };
  }

  private addToHistory(filePath: string, content: string): void {
    if (!this.editHistory.has(filePath)) {
      this.editHistory.set(filePath, []);
    }
    const history = this.editHistory.get(filePath)!;
    history.push(content);
    
    // Keep only last 10 edits
    if (history.length > 10) {
      history.shift();
    }
  }

  private showDiff(oldStr: string, newStr: string): string {
    const oldLines = oldStr.split('\n');
    const newLines = newStr.split('\n');
    
    let diff = '';
    oldLines.forEach(line => {
      diff += chalk.red(`- ${line}\n`);
    });
    newLines.forEach(line => {
      diff += chalk.green(`+ ${line}\n`);
    });
    
    return diff.trim();
  }

  private formatBytes(bytes: number): string {
    if (bytes < 1024) return `${bytes} bytes`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(2)} KB`;
    return `${(bytes / (1024 * 1024)).toFixed(2)} MB`;
  }

  async getRelevantChunks(filePath: string, query: string): Promise<any[]> {
    try {
      if (!fs.existsSync(filePath)) {
        return [];
      }

      const content = fs.readFileSync(filePath, 'utf-8');
      const chunk = ChunkLocalizer.findRelevantChunk(content, query);
      
      if (!chunk) {
        return [];
      }

      return [{
        startLine: chunk.startLine,
        endLine: chunk.endLine,
        content: chunk.content
      }];
    } catch (error) {
      return [];
    }
  }
}

// Chunk localizer for finding relevant code sections
export class ChunkLocalizer {
  static findRelevantChunk(content: string, searchPattern: string, maxLines: number = 50): {
    startLine: number;
    endLine: number;
    content: string;
  } | null {
    const lines = content.split('\n');
    const searchLower = searchPattern.toLowerCase();
    
    let bestMatch = { score: 0, index: -1 };
    
    // Find best matching line
    lines.forEach((line, index) => {
      const score = this.calculateSimilarity(line.toLowerCase(), searchLower);
      if (score > bestMatch.score) {
        bestMatch = { score, index };
      }
    });
    
    if (bestMatch.index === -1 || bestMatch.score < 0.3) {
      return null;
    }
    
    // Extract chunk around best match
    const halfLines = Math.floor(maxLines / 2);
    const startLine = Math.max(0, bestMatch.index - halfLines);
    const endLine = Math.min(lines.length, bestMatch.index + halfLines);
    
    return {
      startLine: startLine + 1,
      endLine: endLine,
      content: lines.slice(startLine, endLine).join('\n')
    };
  }
  
  private static calculateSimilarity(str1: string, str2: string): number {
    const longer = str1.length > str2.length ? str1 : str2;
    const shorter = str1.length > str2.length ? str2 : str1;
    
    if (longer.length === 0) return 1.0;
    
    const distance = this.levenshteinDistance(longer, shorter);
    return (longer.length - distance) / longer.length;
  }
  
  private static levenshteinDistance(str1: string, str2: string): number {
    const matrix: number[][] = [];
    
    for (let i = 0; i <= str2.length; i++) {
      matrix[i] = [i];
    }
    
    for (let j = 0; j <= str1.length; j++) {
      matrix[0][j] = j;
    }
    
    for (let i = 1; i <= str2.length; i++) {
      for (let j = 1; j <= str1.length; j++) {
        if (str2.charAt(i - 1) === str1.charAt(j - 1)) {
          matrix[i][j] = matrix[i - 1][j - 1];
        } else {
          matrix[i][j] = Math.min(
            matrix[i - 1][j - 1] + 1,
            matrix[i][j - 1] + 1,
            matrix[i - 1][j] + 1
          );
        }
      }
    }
    
    return matrix[str2.length][str1.length];
  }
}