'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import AccountList from '@/components/AccountList';

export default function AccountsPage() {
  const router = useRouter();

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Accounts</h1>
          <p className="text-sm text-gray-600 mt-1">
            View and manage customer accounts
          </p>
        </div>
        <Button onClick={() => router.push('/accounts/new')}>Create Account</Button>
      </div>

      <AccountList />
    </div>
  );
}
