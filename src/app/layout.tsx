import type { Metadata } from 'next';
import './globals.css';
import Link from 'next/link';
import { AuthProvider } from '@/contexts/AuthContext';

export const metadata: Metadata = {
  title: 'Card Account Transaction Management',
  description: 'Bill payment and account management system',
};

export default function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  return (
    <html lang="en">
      <body>
        <AuthProvider>
          <div className="min-h-screen bg-gray-50">
            <nav className="bg-white shadow-sm border-b">
              <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
                <div className="flex justify-between h-16">
                  <div className="flex">
                    <Link href="/" className="flex items-center px-2 text-gray-900 hover:text-gray-700">
                      <span className="text-xl font-bold">💳 Card Account Management</span>
                    </Link>
                    <div className="hidden sm:ml-6 sm:flex sm:space-x-8">
                      <Link
                        href="/accounts"
                        className="inline-flex items-center px-1 pt-1 text-sm font-medium text-gray-900 hover:text-gray-700"
                      >
                        Accounts
                      </Link>
                      <Link
                        href="/bill-payment"
                        className="inline-flex items-center px-1 pt-1 text-sm font-medium text-gray-900 hover:text-gray-700"
                      >
                        Bill Payment
                      </Link>
                      <Link
                        href="/transactions"
                        className="inline-flex items-center px-1 pt-1 text-sm font-medium text-gray-900 hover:text-gray-700"
                      >
                        Transactions
                      </Link>
                      <Link
                        href="/card-cross-references"
                        className="inline-flex items-center px-1 pt-1 text-sm font-medium text-gray-900 hover:text-gray-700"
                      >
                        Card Links
                      </Link>
                    </div>
                  </div>
                </div>
              </div>
            </nav>
            <main>{children}</main>
          </div>
        </AuthProvider>
      </body>
    </html>
  );
}
