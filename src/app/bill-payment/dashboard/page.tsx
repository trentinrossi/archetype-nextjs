'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

export default function BillPaymentDashboardPage() {
  const router = useRouter();

  const features = [
    {
      title: 'Make a Payment',
      description: 'Process a full balance bill payment for your account',
      icon: '💳',
      action: () => router.push('/bill-payment'),
      color: 'bg-blue-50 border-blue-200 hover:bg-blue-100',
      buttonColor: 'primary',
    },
    {
      title: 'Payment History',
      description: 'View your past bill payment transactions',
      icon: '📋',
      action: () => router.push('/bill-payment/history'),
      color: 'bg-green-50 border-green-200 hover:bg-green-100',
      buttonColor: 'secondary',
    },
  ];

  return (
    <div className="p-6 max-w-6xl mx-auto">
      <div className="mb-8">
        <h1 className="text-4xl font-bold mb-2">Bill Payment Center</h1>
        <p className="text-gray-600 text-lg">
          Manage your credit card bill payments and view transaction history
        </p>
      </div>

      {/* Feature Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
        {features.map((feature, index) => (
          <div
            key={index}
            className={`border rounded-lg p-6 transition-colors ${feature.color}`}
          >
            <div className="text-5xl mb-4">{feature.icon}</div>
            <h2 className="text-2xl font-bold mb-2">{feature.title}</h2>
            <p className="text-gray-700 mb-4">{feature.description}</p>
            <Button
              onClick={feature.action}
              variant={feature.buttonColor as any}
            >
              {feature.title}
            </Button>
          </div>
        ))}
      </div>

      {/* Information Section */}
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-2xl font-bold mb-4">How Bill Payment Works</h2>
        <div className="space-y-4">
          <div className="flex items-start gap-3">
            <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold">
              1
            </div>
            <div>
              <h3 className="font-semibold text-lg">Enter Account Information</h3>
              <p className="text-gray-600">
                Provide your Account ID (11 characters) and Card Number (16 digits) to begin
              </p>
            </div>
          </div>
          <div className="flex items-start gap-3">
            <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold">
              2
            </div>
            <div>
              <h3 className="font-semibold text-lg">Review Your Balance</h3>
              <p className="text-gray-600">
                View your current account balance and verify the payment amount
              </p>
            </div>
          </div>
          <div className="flex items-start gap-3">
            <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold">
              3
            </div>
            <div>
              <h3 className="font-semibold text-lg">Confirm Payment</h3>
              <p className="text-gray-600">
                Confirm to process a full balance payment. Your account balance will be set to $0.00
              </p>
            </div>
          </div>
          <div className="flex items-start gap-3">
            <div className="flex-shrink-0 w-8 h-8 bg-blue-500 text-white rounded-full flex items-center justify-center font-bold">
              4
            </div>
            <div>
              <h3 className="font-semibold text-lg">Receive Confirmation</h3>
              <p className="text-gray-600">
                Get a unique Transaction ID for your records and view complete payment details
              </p>
            </div>
          </div>
        </div>
      </div>

      {/* Important Information */}
      <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-6 mb-6">
        <h2 className="text-xl font-bold mb-3 text-yellow-900">Important Information</h2>
        <ul className="space-y-2 text-yellow-800">
          <li className="flex items-start gap-2">
            <span className="text-yellow-600 font-bold">•</span>
            <span>
              <strong>Full Balance Payment:</strong> This system processes full balance payments only. 
              Your entire current balance will be paid.
            </span>
          </li>
          <li className="flex items-start gap-2">
            <span className="text-yellow-600 font-bold">•</span>
            <span>
              <strong>Account Validation:</strong> Your account must have a positive balance to process a payment.
            </span>
          </li>
          <li className="flex items-start gap-2">
            <span className="text-yellow-600 font-bold">•</span>
            <span>
              <strong>Transaction ID:</strong> Keep your Transaction ID for your records. 
              You can use it to view payment details later.
            </span>
          </li>
          <li className="flex items-start gap-2">
            <span className="text-yellow-600 font-bold">•</span>
            <span>
              <strong>Payment History:</strong> All completed payments are recorded and can be viewed 
              in the Payment History section.
            </span>
          </li>
        </ul>
      </div>

      {/* Quick Stats */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="bg-white shadow rounded-lg p-4 text-center">
          <div className="text-3xl font-bold text-blue-600">24/7</div>
          <div className="text-gray-600 mt-1">Available</div>
        </div>
        <div className="bg-white shadow rounded-lg p-4 text-center">
          <div className="text-3xl font-bold text-green-600">Instant</div>
          <div className="text-gray-600 mt-1">Processing</div>
        </div>
        <div className="bg-white shadow rounded-lg p-4 text-center">
          <div className="text-3xl font-bold text-purple-600">Secure</div>
          <div className="text-gray-600 mt-1">Transactions</div>
        </div>
      </div>
    </div>
  );
}
