import type * as Preset from "@docusaurus/preset-classic";
import type { Config } from "@docusaurus/types";
import { themes as prismThemes } from "prism-react-renderer";

const config: Config = {
  title: "Hanzo",
  tagline: "Code Less, Make More",
  favicon: "img/logo-square.png",

  // Set the production url of your site here
  url: "https://docs.hanzo.ai",
  baseUrl: "/",

  // GitHub pages deployment config.
  organizationName: "hanzoai",
  projectName: "Hanzo",
  trailingSlash: false,

  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'en',
    locales: ['en', 'fr', 'zh-Hans'],
    localeConfigs: {
      en: {
        htmlLang: 'en-GB',
      },
    },
  },

  markdown: {
    mermaid: true,
  },
  themes: ['@docusaurus/theme-mermaid'],
  presets: [
    [
      "classic",
      {
        docs: {
          path: "modules",
          routeBasePath: "modules",
          sidebarPath: "./sidebars.ts",
          exclude: [
            // '**/_*.{js,jsx,ts,tsx,md,mdx}',
            // '**/_*/**',
            "**/*.test.{js,jsx,ts,tsx}",
            "**/__tests__/**",
          ],
        },
        blog: {
          showReadingTime: true,
        },
        theme: {
          customCss: "./src/css/custom.css",
        },
      } satisfies Preset.Options,
    ],
  ],
  themeConfig: {
    image: "img/docusaurus.png",
    navbar: {
      title: "Hanzo",
      logo: {
        alt: "Hanzo",
        src: "img/logo.png",
      },
      items: [
        {
          type: "docSidebar",
          sidebarId: "docsSidebar",
          position: "left",
          label: "User Guides",
        },
        {
          type: "docSidebar",
          sidebarId: "apiSidebar",
          position: "left",
          label: "Python API",
        },
        {
          type: 'localeDropdown',
          position: 'left',
        },
        {
          href: "https://hanzo.ai",
          label: "Company",
          position: "right",
        },
        {
          href: "https://github.com/hanzoai/build",
          label: "GitHub",
          position: "right",
        },
      ],
    },
    prism: {
      theme: prismThemes.oneLight,
      darkTheme: prismThemes.oneDark,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
