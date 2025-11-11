'use client';

import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

export default function HomePage() {
  const router = useRouter();

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100">
      <div className="container mx-auto px-4 py-16">
        <div className="text-center mb-12">
          <h1 className="text-5xl font-bold text-gray-900 mb-4">
            Account Data Management System
          </h1>
          <p className="text-xl text-gray-600 mb-2">
            CBACT01C - Account Data File Reader and Printer
          </p>
          <p className="text-lg text-gray-500">
            Modern web interface for managing customer account information
          </p>
        </div>

        <div className="max-w-4xl mx-auto grid grid-cols-1 md:grid-cols-2 gap-6">
          {/* Account Management Card */}
          <div className="bg-white rounded-lg shadow-lg p-8 hover:shadow-xl transition-shadow">
            <div className="flex items-center mb-4">
              <div className="w-12 h-12 bg-blue-500 rounded-lg flex items-center justify-center text-white text-2xl font-bold mr-4">
                A
              </div>
              <h2 className="text-2xl font-bold text-gray-900">
                Account Management
              </h2>
            </div>
            <p className="text-gray-600 mb-6">
              View, create, edit, and manage customer accounts with complete
              financial information including balances, credit limits, and cycle data.
            </p>
            <Button
              onClick={() => router.push('/accounts')}
              className="w-full"
            >
              Manage Accounts
            </Button>
          </div>

          {/* Features Card */}
          <div className="bg-white rounded-lg shadow-lg p-8">
            <h2 className="text-2xl font-bold text-gray-900 mb-4">
              Key Features
            </h2>
            <ul className="space-y-3 text-gray-600">
              <li className="flex items-start">
                <span className="text-green-500 mr-2">✓</span>
                <span>Full CRUD operations for account records</span>
              </li>
              <li className="flex items-start">
                <span className="text-green-500 mr-2">✓</span>
                <span>Sequential account processing (BR-001)</span>
              </li>
              <li className="flex items-start">
                <span className="text-green-500 mr-2">✓</span>
                <span>Paginated account listing with filters</span>
              </li>
              <li className="flex items-start">
                <span className="text-green-500 mr-2">✓</span>
                <span>Active and expired account filtering</span>
              </li>
              <li className="flex items-start">
                <span className="text-green-500 mr-2">✓</span>
                <span>Real-time credit availability calculations</span>
              </li>
              <li className="flex items-start">
                <span className="text-green-500 mr-2">✓</span>
                <span>Comprehensive validation and error handling</span>
              </li>
            </ul>
          </div>

          {/* Business Rules Card */}
          <div className="bg-white rounded-lg shadow-lg p-8">
            <h2 className="text-2xl font-bold text-gray-900 mb-4">
              Business Rules
            </h2>
            <div className="space-y-3 text-sm">
              <div className="border-l-4 border-blue-500 pl-4">
                <p className="font-semibold text-gray-900">BR-001</p>
                <p className="text-gray-600">
                  Sequential Account Record Processing
                </p>
              </div>
              <div className="border-l-4 border-green-500 pl-4">
                <p className="font-semibold text-gray-900">BR-002</p>
                <p className="text-gray-600">
                  Account Data Display Requirements
                </p>
              </div>
              <div className="border-l-4 border-yellow-500 pl-4">
                <p className="font-semibold text-gray-900">BR-003</p>
                <p className="text-gray-600">
                  Account File Access Control
                </p>
              </div>
              <div className="border-l-4 border-purple-500 pl-4">
                <p className="font-semibold text-gray-900">BR-004</p>
                <p className="text-gray-600">
                  End of File Detection
                </p>
              </div>
            </div>
          </div>

          {/* System Information Card */}
          <div className="bg-white rounded-lg shadow-lg p-8">
            <h2 className="text-2xl font-bold text-gray-900 mb-4">
              System Information
            </h2>
            <div className="space-y-3 text-gray-600">
              <div>
                <p className="font-semibold text-gray-900">Application</p>
                <p>CBACT01C</p>
              </div>
              <div>
                <p className="font-semibold text-gray-900">Description</p>
                <p>Account Data File Reader and Printer</p>
              </div>
              <div>
                <p className="font-semibold text-gray-900">Technology Stack</p>
                <p>Next.js 15, React 19, TypeScript, TailwindCSS v4</p>
              </div>
              <div>
                <p className="font-semibold text-gray-900">Backend API</p>
                <p>Spring Boot REST API with PostgreSQL</p>
              </div>
            </div>
          </div>
        </div>

        {/* Quick Actions */}
        <div className="max-w-4xl mx-auto mt-8">
          <div className="bg-white rounded-lg shadow-lg p-6">
            <h3 className="text-lg font-semibold text-gray-900 mb-4">
              Quick Actions
            </h3>
            <div className="flex flex-wrap gap-3">
              <Button
                onClick={() => router.push('/accounts')}
                variant="primary"
              >
                View All Accounts
              </Button>
              <Button
                onClick={() => router.push('/accounts/new')}
                variant="primary"
              >
                Create New Account
              </Button>
              <Button
                onClick={() => router.push('/widgets')}
                variant="secondary"
              >
                View Widgets (Example)
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
