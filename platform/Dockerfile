# Hanzo Platform API - Docker Build
# Compute pools and marketplace services

FROM node:20-alpine AS builder

WORKDIR /app

# Install dependencies
COPY package*.json ./
RUN npm ci

# Copy source
COPY . .

# Build (if applicable)
RUN npm run build --if-present

# Production image
FROM node:20-alpine AS runner

WORKDIR /app

ENV NODE_ENV=production

# Create non-root user
RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 hanzo

# Copy built application
COPY --from=builder --chown=hanzo:nodejs /app/package*.json ./
COPY --from=builder --chown=hanzo:nodejs /app/node_modules ./node_modules
COPY --from=builder --chown=hanzo:nodejs /app/dist ./dist
COPY --from=builder --chown=hanzo:nodejs /app/db ./db

USER hanzo

EXPOSE 8000

ENV PORT=8000

CMD ["node", "dist/index.js"]
