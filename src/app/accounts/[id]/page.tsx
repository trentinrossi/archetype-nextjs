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
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.id) {
      fetchAccount(params.id as string);
    }
  }, [params.id]);

  const fetchAccount = async (id: string) => {
    try {
      setLoading(true);
      const data = await accountService.getAccountById(id);
      setAccount(data);
      setError(null);
    } catch (err) {
      setError('Failed to load account');
      console.error(err);
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

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(value);
  };

  const formatDate = (dateString: string | null) => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US');
  };

  const formatDateTime = (dateString: string) => {
    return new Date(dateString).toLocaleString('en-US');
  };

  if (loading) return <div className="p-6">Loading account...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;
  if (!account) return <div className="p-6">Account not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/accounts/${account.acctId}/edit`)}>
            Edit Account
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete Account
          </Button>
          <Button variant="secondary" onClick={() => router.push('/accounts')}>
            Back to List
          </Button>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6 space-y-6">
        {/* Account Identification */}
        <div>
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Account Identification
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Account ID
              </label>
              <p className="mt-1 text-gray-900 font-mono">{account.acctId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Status
              </label>
              <p className="mt-1">
                <span
                  className={`px-3 py-1 rounded text-sm font-semibold ${
                    account.isActive
                      ? 'bg-green-100 text-green-800'
                      : 'bg-gray-100 text-gray-800'
                  }`}
                >
                  {account.activeStatusDisplayName}
                </span>
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Group ID
              </label>
              <p className="mt-1 text-gray-900">{account.acctGroupId || 'N/A'}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Account Expired
              </label>
              <p className="mt-1">
                <span
                  className={`px-3 py-1 rounded text-sm font-semibold ${
                    account.isExpired
                      ? 'bg-red-100 text-red-800'
                      : 'bg-green-100 text-green-800'
                  }`}
                >
                  {account.isExpired ? 'Yes' : 'No'}
                </span>
              </p>
            </div>
          </div>
        </div>

        {/* Financial Information */}
        <div>
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Financial Information
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Current Balance
              </label>
              <p className="mt-1 text-gray-900 text-lg font-semibold">
                {formatCurrency(account.acctCurrBal)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Credit Limit
              </label>
              <p className="mt-1 text-gray-900 text-lg">
                {formatCurrency(account.acctCreditLimit)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Available Credit
              </label>
              <p
                className={`mt-1 text-lg font-semibold ${
                  account.availableCredit < 0 ? 'text-red-600' : 'text-green-600'
                }`}
              >
                {formatCurrency(account.availableCredit)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Cash Credit Limit
              </label>
              <p className="mt-1 text-gray-900 text-lg">
                {formatCurrency(account.acctCashCreditLimit)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Available Cash Credit
              </label>
              <p
                className={`mt-1 text-lg font-semibold ${
                  account.availableCashCredit < 0
                    ? 'text-red-600'
                    : 'text-green-600'
                }`}
              >
                {formatCurrency(account.availableCashCredit)}
              </p>
            </div>
          </div>
        </div>

        {/* Current Cycle Information */}
        <div>
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Current Cycle Information
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Current Cycle Credit
              </label>
              <p className="mt-1 text-gray-900 text-lg">
                {formatCurrency(account.acctCurrCycCredit)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Current Cycle Debit
              </label>
              <p className="mt-1 text-gray-900 text-lg">
                {formatCurrency(account.acctCurrCycDebit)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Current Cycle Net Amount
              </label>
              <p
                className={`mt-1 text-lg font-semibold ${
                  account.currentCycleNetAmount < 0
                    ? 'text-red-600'
                    : 'text-green-600'
                }`}
              >
                {formatCurrency(account.currentCycleNetAmount)}
              </p>
            </div>
          </div>
        </div>

        {/* Date Information */}
        <div>
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            Date Information
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Open Date
              </label>
              <p className="mt-1 text-gray-900">{formatDate(account.acctOpenDate)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Expiration Date
              </label>
              <p className="mt-1 text-gray-900">
                {formatDate(account.acctExpirationDate)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Reissue Date
              </label>
              <p className="mt-1 text-gray-900">
                {formatDate(account.acctReissueDate)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Has Been Reissued
              </label>
              <p className="mt-1">
                <span
                  className={`px-3 py-1 rounded text-sm font-semibold ${
                    account.hasBeenReissued
                      ? 'bg-blue-100 text-blue-800'
                      : 'bg-gray-100 text-gray-800'
                  }`}
                >
                  {account.hasBeenReissued ? 'Yes' : 'No'}
                </span>
              </p>
            </div>
          </div>
        </div>

        {/* System Information */}
        <div>
          <h2 className="text-lg font-semibold mb-4 border-b pb-2">
            System Information
          </h2>
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Created At
              </label>
              <p className="mt-1 text-gray-900 text-sm">
                {formatDateTime(account.createdAt)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700">
                Updated At
              </label>
              <p className="mt-1 text-gray-900 text-sm">
                {formatDateTime(account.updatedAt)}
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
