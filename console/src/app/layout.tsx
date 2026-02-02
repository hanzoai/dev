import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import Link from 'next/link'
import './globals.css'
import { Providers } from './providers'
import {
  LayoutDashboard,
  Server,
  Container,
  Layers,
  Settings,
  Activity,
} from 'lucide-react'

const inter = Inter({ subsets: ['latin'], variable: '--font-sans' })

export const metadata: Metadata = {
  title: 'Hanzo Console',
  description: 'Admin dashboard for Hanzo Node management',
}

const navItems = [
  { href: '/', label: 'Dashboard', icon: LayoutDashboard },
  { href: '/deployments', label: 'Deployments', icon: Container },
  { href: '/pools', label: 'Compute Pools', icon: Layers },
  { href: '/nodes', label: 'Nodes', icon: Server },
  { href: '/challenges', label: 'QoS Challenges', icon: Activity },
  { href: '/settings', label: 'Settings', icon: Settings },
]

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" className="dark">
      <body className={`${inter.variable} font-sans antialiased`}>
        <Providers>
          <div className="flex min-h-screen">
            {/* Sidebar */}
            <aside className="hidden lg:flex lg:w-64 lg:flex-col lg:fixed lg:inset-y-0">
              <div className="flex flex-col flex-grow bg-card border-r border-border">
                {/* Logo */}
                <div className="flex items-center h-16 px-6 border-b border-border">
                  <Link href="/" className="flex items-center gap-2">
                    <div className="w-8 h-8 rounded-lg bg-primary flex items-center justify-center">
                      <span className="text-primary-foreground font-bold text-lg">H</span>
                    </div>
                    <span className="font-semibold text-lg">Hanzo Console</span>
                  </Link>
                </div>

                {/* Navigation */}
                <nav className="flex-1 px-4 py-6 space-y-1">
                  {navItems.map((item) => (
                    <Link
                      key={item.href}
                      href={item.href}
                      className="flex items-center gap-3 px-3 py-2 text-sm font-medium rounded-md text-muted-foreground hover:text-foreground hover:bg-accent transition-colors"
                    >
                      <item.icon className="h-5 w-5" />
                      {item.label}
                    </Link>
                  ))}
                </nav>

                {/* Footer */}
                <div className="p-4 border-t border-border">
                  <div className="flex items-center gap-2 text-xs text-muted-foreground">
                    <div className="w-2 h-2 rounded-full bg-green-500 animate-pulse" />
                    <span>Node Connected</span>
                  </div>
                </div>
              </div>
            </aside>

            {/* Main content */}
            <main className="flex-1 lg:pl-64">
              <div className="p-6 lg:p-8">{children}</div>
            </main>
          </div>
        </Providers>
      </body>
    </html>
  )
}
