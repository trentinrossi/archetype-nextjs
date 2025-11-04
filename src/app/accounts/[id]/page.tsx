'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account } from '@/types/account';
import { Button } from '@/components/ui';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (params.id) {
      fetchAccount(params.id as string);
    }
  }, [params.id]);

  const fetchAccount = async (id: string) => {
    try {
      const data = await accountService.getAccountById(id);
      setAccount(data);
    } catch (err) {
      console.error('Failed to load account:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this account?')) return;

    try {
      await accountService.deleteAccount(params.id as string);
      router.push('/accounts');
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (!account) return <div className="p-6">Account not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/accounts/${account.accountId}/edit`)}>
            Edit
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/accounts')}>
            Back to List
          </Button>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6 space-y-4">
        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-700">Account ID</label>
            <p className="mt-1 text-gray-900">{account.accountId}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Active Status</label>
            <p className="mt-1 text-gray-900">{account.activeStatus}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Current Balance</label>
            <p className="mt-1 text-gray-900">${account.currentBalance.toFixed(2)}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Credit Limit</label>
            <p className="mt-1 text-gray-900">${account.creditLimit.toFixed(2)}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Cash Credit Limit</label>
            <p className="mt-1 text-gray-900">${account.cashCreditLimit.toFixed(2)}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Group ID</label>
            <p className="mt-1 text-gray-900">{account.groupId}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Open Date</label>
            <p className="mt-1 text-gray-900">{account.openDate}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Expiration Date</label>
            <p className="mt-1 text-gray-900">{account.expirationDate}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Current Cycle Credit</label>
            <p className="mt-1 text-gray-900">${account.currentCycleCredit.toFixed(2)}</p>
          </div>
          <div>
            <label className="block text-sm font-semibold text-gray-700">Current Cycle Debit</label>
            <p className="mt-1 text-gray-900">${account.currentCycleDebit.toFixed(2)}</p>
          </div>
        </div>
      </div>
    </div>
  );
}
