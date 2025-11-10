'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

export default function HomePage() {
  const router = useRouter();

  const menuItems = [
    {
      title: 'Card Management',
      description: 'View, create, and manage credit cards',
      icon: '💳',
      path: '/cards',
      color: 'bg-blue-50 hover:bg-blue-100 border-blue-200',
    },
    {
      title: 'Account Management',
      description: 'View, create, and manage accounts',
      icon: '🏦',
      path: '/accounts',
      color: 'bg-green-50 hover:bg-green-100 border-green-200',
    },
  ];

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <div className="bg-white shadow">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          <h1 className="text-3xl font-bold text-gray-900">
            Card Services Management System
          </h1>
          <p className="mt-2 text-sm text-gray-600">
            Account and Payment Processing Application
          </p>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Welcome Section */}
        <div className="bg-white shadow rounded-lg p-6 mb-8">
          <h2 className="text-xl font-semibold mb-2">Welcome</h2>
          <p className="text-gray-600">
            This system provides comprehensive card services including account management,
            card management, and payment processing functionality. Select an option below
            to get started.
          </p>
        </div>

        {/* Menu Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {menuItems.map((item) => (
            <button
              key={item.path}
              onClick={() => router.push(item.path)}
              className={`${item.color} border-2 rounded-lg p-6 text-left transition-all duration-200 transform hover:scale-105 hover:shadow-lg`}
            >
              <div className="flex items-start space-x-4">
                <div className="text-4xl">{item.icon}</div>
                <div className="flex-1">
                  <h3 className="text-lg font-semibold text-gray-900 mb-2">
                    {item.title}
                  </h3>
                  <p className="text-sm text-gray-600">{item.description}</p>
                </div>
                <div className="text-gray-400">
                  <svg
                    className="w-6 h-6"
                    fill="none"
                    stroke="currentColor"
                    viewBox="0 0 24 24"
                  >
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M9 5l7 7-7 7"
                    />
                  </svg>
                </div>
              </div>
            </button>
          ))}
        </div>

        {/* Quick Actions */}
        <div className="mt-8 bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Quick Actions</h2>
          <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-4">
            <Button
              onClick={() => router.push('/cards/new')}
              className="w-full"
            >
              Create New Card
            </Button>
            <Button
              onClick={() => router.push('/accounts/new')}
              className="w-full"
            >
              Create New Account
            </Button>
            <Button
              onClick={() => router.push('/cards')}
              variant="secondary"
              className="w-full"
            >
              View All Cards
            </Button>
            <Button
              onClick={() => router.push('/accounts')}
              variant="secondary"
              className="w-full"
            >
              View All Accounts
            </Button>
          </div>
        </div>

        {/* System Information */}
        <div className="mt-8 bg-blue-50 border border-blue-200 rounded-lg p-6">
          <h2 className="text-lg font-semibold text-blue-900 mb-2">
            ℹ️ System Information
          </h2>
          <div className="text-sm text-blue-800 space-y-2">
            <p>
              <strong>Application:</strong> COCRDLIC - Credit Card List Program
            </p>
            <p>
              <strong>Version:</strong> 1.0.0
            </p>
            <p>
              <strong>Features:</strong> Account Management, Card Management, Payment Processing
            </p>
          </div>
        </div>

        {/* Help Section */}
        <div className="mt-8 bg-gray-100 rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Need Help?</h2>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm">
            <div>
              <h3 className="font-semibold text-gray-900 mb-2">Card Management</h3>
              <ul className="text-gray-600 space-y-1">
                <li>• View all credit cards</li>
                <li>• Filter by account or card number</li>
                <li>• Create, edit, and delete cards</li>
                <li>• View card details and status</li>
              </ul>
            </div>
            <div>
              <h3 className="font-semibold text-gray-900 mb-2">Account Management</h3>
              <ul className="text-gray-600 space-y-1">
                <li>• View all accounts</li>
                <li>• Filter by account status</li>
                <li>• Create, edit, and delete accounts</li>
                <li>• View account details and balances</li>
              </ul>
            </div>
            <div>
              <h3 className="font-semibold text-gray-900 mb-2">Navigation</h3>
              <ul className="text-gray-600 space-y-1">
                <li>• Click on menu items to navigate</li>
                <li>• Use action buttons on each row</li>
                <li>• Use breadcrumb navigation</li>
                <li>• Return to main menu anytime</li>
              </ul>
            </div>
          </div>
        </div>
      </div>

      {/* Footer */}
      <div className="bg-white border-t mt-12">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          <p className="text-center text-sm text-gray-500">
            Card Services Account and Payment Processing System
          </p>
        </div>
      </div>
    </div>
  );
}
