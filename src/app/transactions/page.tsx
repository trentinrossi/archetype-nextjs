'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import TransactionList from '@/components/TransactionList';

export default function TransactionsPage() {
  const router = useRouter();

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Transactions</h1>
          <p className="text-sm text-gray-600 mt-1">
            View and manage all card transactions
          </p>
        </div>
        <Button onClick={() => router.push('/transactions/new')}>Create Transaction</Button>
      </div>

      <TransactionList />
    </div>
  );
}
