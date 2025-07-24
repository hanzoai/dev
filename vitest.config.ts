import { defineConfig } from 'vitest/config';
import path from 'path';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['tests/**/*.test.ts'],
    exclude: ['node_modules', 'dist', 'build'],
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      exclude: [
        'node_modules',
        'tests',
        'dist',
        '**/*.d.ts',
        '**/*.config.*',
        '**/mockData.ts'
      ]
    },
    testTimeout: 5000,
    hookTimeout: 5000,
    pool: 'threads',
    poolOptions: {
      threads: {
        singleThread: true
      }
    },
    forceRerunTriggers: ['**/*.test.ts']
  },
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src')
    }
  }
});