'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

export default function HomePage() {
  const router = useRouter();

  const features = [
    {
      title: 'Credit Card Management',
      description: 'View, search, create, and update credit cards with comprehensive validation and authorization controls.',
      actions: [
        { label: 'View All Cards', path: '/credit-cards' },
        { label: 'Search Cards', path: '/credit-cards/search' },
        { label: 'Create New Card', path: '/credit-cards/new' },
      ],
    },
    {
      title: 'Account Management',
      description: 'Manage credit card accounts and view associated cards.',
      actions: [
        { label: 'View All Accounts', path: '/accounts' },
        { label: 'Create New Account', path: '/accounts/new' },
      ],
    },
    {
      title: 'Transaction Management',
      description: 'View and manage credit card transactions with advanced filtering and reporting.',
      actions: [
        { label: 'View Transactions', path: '/transactions' },
        { label: 'Create Transaction', path: '/transactions/new' },
        { label: 'Transaction Reports', path: '/transactions/reports' },
      ],
    },
  ];

  return (
    <div className="p-6">
      <div className="mb-8">
        <h1 className="text-3xl font-bold mb-2">Card Management System</h1>
        <p className="text-gray-600">
          Comprehensive credit card and account management with robust authorization and validation
        </p>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {features.map((feature, index) => (
          <div
            key={index}
            className="bg-white shadow rounded-lg p-6 hover:shadow-lg transition-shadow"
          >
            <h2 className="text-xl font-semibold mb-3">{feature.title}</h2>
            <p className="text-gray-600 mb-4 text-sm">{feature.description}</p>
            <div className="space-y-2">
              {feature.actions.map((action, actionIndex) => (
                <Button
                  key={actionIndex}
                  onClick={() => router.push(action.path)}
                  variant={actionIndex === 0 ? 'primary' : 'secondary'}
                  className="w-full"
                >
                  {action.label}
                </Button>
              ))}
            </div>
          </div>
        ))}
      </div>

      <div className="mt-8 bg-blue-50 border border-blue-200 rounded-lg p-6">
        <h3 className="text-lg font-semibold mb-2">System Features</h3>
        <ul className="list-disc list-inside space-y-1 text-sm text-gray-700">
          <li>User authorization controls (Admin vs Regular users)</li>
          <li>Comprehensive input validation and error handling</li>
          <li>Concurrent update prevention with optimistic locking</li>
          <li>Paginated list views with filtering capabilities</li>
          <li>Card number masking for security</li>
          <li>Real-time expiration status tracking</li>
          <li>Audit trail with last modified by tracking</li>
        </ul>
      </div>

      <div className="mt-6 bg-yellow-50 border border-yellow-200 rounded-lg p-6">
        <h3 className="text-lg font-semibold mb-2">Business Rules Implemented</h3>
        <ul className="list-disc list-inside space-y-1 text-sm text-gray-700">
          <li><strong>BR001:</strong> User authorization for card viewing based on user type</li>
          <li><strong>BR002:</strong> Card detail modification (name, status, expiration only)</li>
          <li><strong>BR003:</strong> Concurrent update prevention and search criteria validation</li>
          <li><strong>BR004:</strong> Account and card number validation with proper error messages</li>
          <li><strong>BR006:</strong> Page navigation control with state preservation</li>
          <li><strong>BR007:</strong> Input error highlighting with detailed messages</li>
          <li><strong>BR008:</strong> Record filtering logic based on search criteria</li>
        </ul>
      </div>
    </div>
  );
}
