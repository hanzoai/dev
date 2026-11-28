import { describe, expect, it } from 'vitest'

import { addressOf, brief, folderName, hostOf, siteName, stopped, urlOf, type Saved } from './site.ts'

describe('which words are a site (hanzo-inc/app issue 312)', () => {
  it('takes an address with a scheme as written', () => {
    expect(addressOf('https://example.com/a?b=1#c')).toBe('https://example.com/a?b=1#c')
    expect(addressOf('http://example.com')).toBe('http://example.com/')
    expect(addressOf('HTTPS://Example.COM')).toBe('https://example.com/')
  })

  it('gives a bare name the scheme a site has', () => {
    expect(addressOf('example.com')).toBe('https://example.com/')
    expect(addressOf('www.example.co')).toBe('https://www.example.co/')
    expect(addressOf('blog.example.dev/posts/1')).toBe('https://blog.example.dev/posts/1')
    expect(addressOf('example.com:8443/x')).toBe('https://example.com:8443/x')
  })

  it('takes this computer’s own addresses over http', () => {
    expect(addressOf('localhost:3000')).toBe('http://localhost:3000/')
    expect(addressOf('127.0.0.1:5173/app')).toBe('http://127.0.0.1:5173/app')
  })

  it('leaves out the punctuation of a sentence', () => {
    expect(addressOf('(https://example.com)')).toBe('https://example.com/')
    expect(addressOf('"example.com".')).toBe('https://example.com/')
    expect(addressOf('https://example.com/a,')).toBe('https://example.com/a')
    expect(addressOf('https://en.wikipedia.org/wiki/Foo_(bar)')).toBe('https://en.wikipedia.org/wiki/Foo_(bar)')
    expect(addressOf('https://en.wikipedia.org/wiki/Foo_(bar))')).toBe('https://en.wikipedia.org/wiki/Foo_(bar)')
  })

  it('does not take a file name, a path or a word for a site', () => {
    for (const no of ['README.md', 'index.html', 'main.rs', 'setup.sh', 'app.js', 'package.json', 'src/index.ts', 'hello', 'a.b', 'v1.2', '1.5', '', '   ', 'localhost', 'example.invalidtld']) {
      expect(addressOf(no), no).toBeNull()
    }
  })

  it('does not take an address with a login in it, or a scheme the web has not', () => {
    for (const no of ['https://user:pw@example.com', 'https://user@example.com', 'file:///etc/passwd', 'ftp://example.com', 'javascript:alert(1)']) {
      expect(addressOf(no), no).toBeNull()
    }
  })

  it('does not choke on a very long word', () => {
    expect(addressOf('a'.repeat(5000) + '.com')).toBeNull()
    expect(addressOf('a'.repeat(300) + '!')).toBeNull()
  })
})

describe('when an ask is about a site', () => {
  it('is an address alone', () => {
    expect(urlOf('https://example.com')).toEqual({ url: 'https://example.com/', note: '' })
    expect(urlOf('  example.com  ')).toEqual({ url: 'https://example.com/', note: '' })
    expect(urlOf('localhost:3000')).toEqual({ url: 'http://localhost:3000/', note: '' })
  })

  it('is one address with a request to redesign it, and keeps the rest of what was said', () => {
    expect(urlOf('Redesign https://example.com and make it darker')).toEqual({
      url: 'https://example.com/',
      note: 'Redesign and make it darker',
    })
    expect(urlOf('please redesign example.com.')).toEqual({ url: 'https://example.com/', note: 'please redesign' })
    expect(urlOf('Can you critique www.example.org for me?')).toEqual({ url: 'https://www.example.org/', note: 'Can you critique for me?' })
    expect(urlOf('revamp my site localhost:3000')).toEqual({ url: 'http://localhost:3000/', note: 'revamp my site' })
  })

  it('is not a message that merely holds a link: that is sent as it is', () => {
    expect(urlOf('What is https://example.com?')).toBeNull()
    expect(urlOf('See https://example.com/docs for the API')).toBeNull()
    expect(urlOf('Fix the typo in README.md')).toBeNull()
    expect(urlOf('Add a checkout page')).toBeNull()
    expect(urlOf('')).toBeNull()
  })

  it('is not about two sites at once', () => {
    expect(urlOf('Redesign example.com and example.org')).toBeNull()
    expect(urlOf('example.com example.org')).toBeNull()
  })
})

describe('what the redesign is called', () => {
  it('names the site by its host without www and its top-level name', () => {
    expect(siteName('https://example.com/')).toBe('example')
    expect(siteName('https://www.example.com/a')).toBe('example')
    expect(siteName('https://blog.example.com')).toBe('blog-example')
    expect(siteName('https://shop.example.co.uk')).toBe('shop-example')
    expect(siteName('http://localhost:3000')).toBe('localhost')
    expect(siteName('http://127.0.0.1:8080')).toBe('localhost')
    expect(siteName('http://[::1]:8080/')).toBe('localhost')
    expect(siteName('not an address')).toBe('site')
    expect(folderName('https://www.hanzo.ai')).toBe('hanzo-redesign')
  })

  it('says a site as a person reads it', () => {
    expect(hostOf('https://www.example.com/a/b')).toBe('example.com')
    expect(hostOf('http://localhost:3000/')).toBe('localhost:3000')
  })
})

describe('what the agent is told', () => {
  const saved: Saved = {
    title: 'Acme Plumbing',
    url: 'https://acme.test/',
    files: ['page.html', 'text.md', 'style-1.css', 'screenshot.png'],
    screenshot: true,
  }

  it('names the site, each file saved, and the two things to do', () => {
    const text = brief(saved)
    expect(text).toContain('Redesign the website https://acme.test/ (Acme Plumbing).')
    expect(text).toContain('- .hanzo/reference/text.md: the site’s address, its words in reading order, and the pictures it shows')
    expect(text).toContain('- .hanzo/reference/page.html: the page as the site sent it')
    expect(text).toContain('- .hanzo/reference/style-1.css: one of its stylesheets')
    expect(text).toContain('- .hanzo/reference/screenshot.png: how the page looks')
    expect(text).toContain('1. Critique it.')
    expect(text).toContain('2. Rebuild it.')
    expect(text).toContain('`redesign/`')
    expect(text).toContain('Do not start a server and do not use the network')
    expect(text).not.toContain('The person added')
  })

  it('carries what the person said besides the address', () => {
    expect(brief(saved, '  make it darker ')).toContain('The person added: “make it darker”')
  })

  it('does not list a picture that was not taken, and does not repeat an address as its own title', () => {
    const text = brief({ title: 'https://acme.test/', url: 'https://acme.test/', files: ['page.html', 'text.md'], screenshot: false })
    expect(text).not.toContain('screenshot')
    expect(text).toContain('Redesign the website https://acme.test/.')
  })
})

describe('when a step stops', () => {
  it('says the host’s own words, else that it could not start', () => {
    expect(stopped('The site took too long to answer.')).toBe('The site took too long to answer.')
    expect(stopped(new Error('Hanzo could not find that site. Check the address and try again.'))).toBe('Hanzo could not find that site. Check the address and try again.')
    expect(stopped(undefined)).toBe('The redesign could not start. Try again.')
  })
})
