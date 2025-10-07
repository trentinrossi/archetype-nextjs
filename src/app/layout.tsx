import type { Metadata } from "next";
import { Geist, Geist_Mono } from "next/font/google";
import "./globals.css";

const geistSans = Geist({
  variable: "--font-geist-sans",
  subsets: ["latin"],
});

const geistMono = Geist_Mono({
  variable: "--font-geist-mono",
  subsets: ["latin"],
});

export const metadata: Metadata = {
  title: "CardDemo - User Management System",
  description: "COBOL-to-NextJS modernized User Management System with CICS-style interface",
  keywords: ["user management", "COBOL", "NextJS", "modernization", "CardDemo"],
  authors: [{ name: "Wynxx System Modernization Team" }],
  viewport: "width=device-width, initial-scale=1",
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <head>
        <meta name="theme-color" content="#111827" />
        <meta name="application-name" content="CardDemo User Management" />
        <meta name="description" content="Modernized COBOL User Management System" />
      </head>
      <body
        className={`${geistSans.variable} ${geistMono.variable} antialiased bg-gray-900 text-green-400`}
      >
        <div className="min-h-screen">
          {children}
        </div>
      </body>
    </html>
  );
}