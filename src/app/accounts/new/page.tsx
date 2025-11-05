'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import AccountForm from '@/components/AccountForm';
import { Account } from '@/types/account';

export default function NewAccountPage() {
  const router = useRouter();

  const handleSuccess = (account: Account) => {
    alert(`Account created successfully! Account ID: ${account.accountId}`);
    router.push('/accounts');
  };

  const handleCancel = () => {
    router.push('/accounts');
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
        <h1 className="text-2xl font-bold text-gray-900">Create New Account</h1>
        <p className="text-sm text-gray-600 mt-1">
          Fill in the account details below
        </p>
      </div>

      <AccountForm onSuccess={handleSuccess} onCancel={handleCancel} />
    </div>
  );
}
