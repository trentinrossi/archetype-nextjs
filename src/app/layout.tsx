import type { Metadata } from 'next';
import './globals.css';
import Link from 'next/link';

export const metadata: Metadata = {
  title: 'Card Management System',
  description: 'Credit card and account management system',
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body className="min-h-screen bg-gray-50">
        <nav className="bg-white shadow-sm border-b border-gray-200">
          <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
            <div className="flex justify-between h-16">
              <div className="flex">
                <Link href="/" className="flex items-center px-2 text-xl font-bold text-gray-900 hover:text-gray-700">
                  Card Management System
                </Link>
                <div className="hidden sm:ml-6 sm:flex sm:space-x-4">
                  <Link
                    href="/credit-cards"
                    className="inline-flex items-center px-3 py-2 text-sm font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50 rounded-md"
                  >
                    Credit Cards
                  </Link>
                  <Link
                    href="/accounts"
                    className="inline-flex items-center px-3 py-2 text-sm font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50 rounded-md"
                  >
                    Accounts
                  </Link>
                  <Link
                    href="/transactions"
                    className="inline-flex items-center px-3 py-2 text-sm font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50 rounded-md"
                  >
                    Transactions
                  </Link>
                  <Link
                    href="/credit-cards/search"
                    className="inline-flex items-center px-3 py-2 text-sm font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50 rounded-md"
                  >
                    Search Cards
                  </Link>
                </div>
              </div>
            </div>
          </div>
        </nav>
        <main className="max-w-7xl mx-auto">
          {children}
        </main>
        <footer className="bg-white border-t border-gray-200 mt-12">
          <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
            <p className="text-center text-sm text-gray-500">
              Card Management System - Credit Card and Account Management
            </p>
          </div>
        </footer>
      </body>
    </html>
  );
}
