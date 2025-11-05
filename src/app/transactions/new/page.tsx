'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import TransactionForm from '@/components/TransactionForm';
import { Transaction } from '@/types/transaction';

export default function NewTransactionPage() {
  const router = useRouter();

  const handleSuccess = (transaction: Transaction) => {
    alert(`Transaction created successfully! Transaction ID: ${transaction.transactionId}`);
    router.push('/transactions');
  };

  const handleCancel = () => {
    router.push('/transactions');
  };

  return (
    <div className="p-6 max-w-5xl mx-auto">
      <div className="mb-6">
        <button
          onClick={() => router.back()}
          className="text-blue-600 hover:text-blue-800 flex items-center gap-2 mb-4"
        >
          <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M15 19l-7-7 7-7" />
          </svg>
          Back
        </button>
        <h1 className="text-2xl font-bold text-gray-900">Create New Transaction</h1>
        <p className="text-sm text-gray-600 mt-1">
          Fill in the transaction details below
        </p>
      </div>

      <TransactionForm onSuccess={handleSuccess} onCancel={handleCancel} />
    </div>
  );
}
