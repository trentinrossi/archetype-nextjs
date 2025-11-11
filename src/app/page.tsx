'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

export default function HomePage() {
  const router = useRouter();

  const features = [
    {
      title: 'Accounts',
      description: 'Manage credit card accounts, view balances, and track account status.',
      icon: '💳',
      path: '/accounts',
      color: 'bg-blue-50 border-blue-200 hover:bg-blue-100',
    },
    {
      title: 'Bill Payment',
      description: 'Process bill payments for accounts with outstanding balances.',
      icon: '💰',
      path: '/bill-payment',
      color: 'bg-green-50 border-green-200 hover:bg-green-100',
    },
    {
      title: 'Transactions',
      description: 'View transaction history, bill payments, and transaction details.',
      icon: '📊',
      path: '/transactions',
      color: 'bg-purple-50 border-purple-200 hover:bg-purple-100',
    },
    {
      title: 'Card Cross References',
      description: 'Link cards to accounts and manage card-account relationships.',
      icon: '🔗',
      path: '/card-cross-references',
      color: 'bg-orange-50 border-orange-200 hover:bg-orange-100',
    },
  ];

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="p-6 max-w-7xl mx-auto">
        <div className="text-center mb-12">
          <h1 className="text-4xl font-bold text-gray-900 mb-4">
            Card Account Transaction Management
          </h1>
          <p className="text-xl text-gray-600">
            Comprehensive bill payment and account management system
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-12">
          {features.map((feature) => (
            <div
              key={feature.path}
              className={`border-2 rounded-lg p-6 cursor-pointer transition-all ${feature.color}`}
              onClick={() => router.push(feature.path)}
            >
              <div className="flex items-start">
                <div className="text-4xl mr-4">{feature.icon}</div>
                <div className="flex-1">
                  <h2 className="text-2xl font-bold mb-2">{feature.title}</h2>
                  <p className="text-gray-700 mb-4">{feature.description}</p>
                  <Button size="sm" onClick={(e) => {
                    e.stopPropagation();
                    router.push(feature.path);
                  }}>
                    Go to {feature.title}
                  </Button>
                </div>
              </div>
            </div>
          ))}
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-2xl font-bold mb-4">System Features</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <h3 className="font-semibold text-lg mb-2">Account Management</h3>
              <ul className="list-disc list-inside text-gray-700 space-y-1">
                <li>Create and manage credit card accounts</li>
                <li>View current balances and account status</li>
                <li>Track account history and updates</li>
                <li>Delete accounts when needed</li>
              </ul>
            </div>
            
            <div>
              <h3 className="font-semibold text-lg mb-2">Bill Payment Processing</h3>
              <ul className="list-disc list-inside text-gray-700 space-y-1">
                <li>Process full balance payments</li>
                <li>Validate account and balance before payment</li>
                <li>Require confirmation before processing</li>
                <li>Generate unique transaction IDs</li>
              </ul>
            </div>
            
            <div>
              <h3 className="font-semibold text-lg mb-2">Transaction Tracking</h3>
              <ul className="list-disc list-inside text-gray-700 space-y-1">
                <li>View all transactions with pagination</li>
                <li>Filter by account or card number</li>
                <li>View detailed transaction information</li>
                <li>Track bill payment history</li>
              </ul>
            </div>
            
            <div>
              <h3 className="font-semibold text-lg mb-2">Card Cross References</h3>
              <ul className="list-disc list-inside text-gray-700 space-y-1">
                <li>Link cards to accounts</li>
                <li>View all card-account relationships</li>
                <li>Manage multiple cards per account</li>
                <li>Remove card links when needed</li>
              </ul>
            </div>
          </div>
        </div>

        <div className="mt-8 bg-blue-50 border border-blue-200 rounded-lg p-6">
          <h2 className="text-xl font-bold mb-2 text-blue-900">Business Rules</h2>
          <ul className="list-disc list-inside text-blue-800 space-y-1">
            <li><strong>BR001:</strong> Account Validation - Validates that the entered account ID exists in the system</li>
            <li><strong>BR002:</strong> Balance Check - Verifies that the account has a positive balance to pay</li>
            <li><strong>BR003:</strong> Payment Confirmation - Requires user confirmation before processing payment</li>
            <li><strong>BR004:</strong> Full Balance Payment - Payment processes the full current account balance</li>
            <li><strong>BR005:</strong> Transaction ID Generation - Generates unique sequential transaction ID</li>
            <li><strong>BR006:</strong> Bill Payment Transaction Recording - Records bill payment with specific transaction attributes</li>
            <li><strong>BR007:</strong> Account Balance Update - Updates account balance after successful payment</li>
          </ul>
        </div>
      </div>
    </div>
  );
}
