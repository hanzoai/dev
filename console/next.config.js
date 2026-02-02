/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'standalone',
  reactStrictMode: true,
  transpilePackages: ['@hanzo/pools-ui'],

  // Environment variables available at build time
  env: {
    HANZO_NODE_URL: process.env.HANZO_NODE_URL || 'http://localhost:8080',
    HANZO_NODE_GRPC_URL: process.env.HANZO_NODE_GRPC_URL || 'http://localhost:9090',
  },

  // API rewrites to proxy requests to hanzo-node
  async rewrites() {
    return [
      {
        source: '/api/node/:path*',
        destination: `${process.env.HANZO_NODE_URL || 'http://localhost:8080'}/:path*`,
      },
    ]
  },

  // Security headers
  async headers() {
    return [
      {
        source: '/:path*',
        headers: [
          { key: 'X-Frame-Options', value: 'DENY' },
          { key: 'X-Content-Type-Options', value: 'nosniff' },
          { key: 'Referrer-Policy', value: 'strict-origin-when-cross-origin' },
        ],
      },
    ]
  },
}

module.exports = nextConfig
