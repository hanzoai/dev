import { type ClassValue, clsx } from 'clsx'
import { twMerge } from 'tailwind-merge'

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}

export function formatBytes(bytes: number, decimals = 2): string {
  if (bytes === 0) return '0 Bytes'
  const k = 1024
  const dm = decimals < 0 ? 0 : decimals
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB']
  const i = Math.floor(Math.log(bytes) / Math.log(k))
  return `${parseFloat((bytes / Math.pow(k, i)).toFixed(dm))} ${sizes[i]}`
}

export function formatUptime(seconds: number): string {
  const days = Math.floor(seconds / 86400)
  const hours = Math.floor((seconds % 86400) / 3600)
  const minutes = Math.floor((seconds % 3600) / 60)

  if (days > 0) {
    return `${days}d ${hours}h ${minutes}m`
  }
  if (hours > 0) {
    return `${hours}h ${minutes}m`
  }
  return `${minutes}m`
}

export function formatTimestamp(timestamp: number): string {
  return new Date(timestamp).toLocaleString()
}

export function formatRelativeTime(timestamp: number): string {
  const now = Date.now()
  const diff = now - timestamp

  const seconds = Math.floor(diff / 1000)
  const minutes = Math.floor(seconds / 60)
  const hours = Math.floor(minutes / 60)
  const days = Math.floor(hours / 24)

  if (days > 0) return `${days}d ago`
  if (hours > 0) return `${hours}h ago`
  if (minutes > 0) return `${minutes}m ago`
  return 'just now'
}

export function formatPercent(value: number, decimals = 1): string {
  return `${(value * 100).toFixed(decimals)}%`
}

export function formatNumber(value: number): string {
  return new Intl.NumberFormat().format(value)
}

export function truncateAddress(address: string, chars = 4): string {
  if (!address) return ''
  return `${address.slice(0, chars + 2)}...${address.slice(-chars)}`
}

export function getStatusColor(status: string): string {
  switch (status.toLowerCase()) {
    case 'healthy':
    case 'active':
    case 'running':
    case 'ready':
    case 'passed':
      return 'text-green-500'
    case 'degraded':
    case 'draining':
    case 'pending':
      return 'text-yellow-500'
    case 'unhealthy':
    case 'offline':
    case 'stopped':
    case 'failed':
    case 'expired':
      return 'text-red-500'
    default:
      return 'text-gray-500'
  }
}

export function getStatusBgColor(status: string): string {
  switch (status.toLowerCase()) {
    case 'healthy':
    case 'active':
    case 'running':
    case 'ready':
    case 'passed':
      return 'bg-green-500/10'
    case 'degraded':
    case 'draining':
    case 'pending':
      return 'bg-yellow-500/10'
    case 'unhealthy':
    case 'offline':
    case 'stopped':
    case 'failed':
    case 'expired':
      return 'bg-red-500/10'
    default:
      return 'bg-gray-500/10'
  }
}
