'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import { accountService } from '@/services/accountService';
import { Account } from '@/types/account';

const AccountDetailPage: React.FC = () => {
  const params = useParams();
  const router = useRouter();
  const accountId = params.id as string;
  
  const [account, setAccount] = useState<Account | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchAccount = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await accountService.getAccountById(accountId);
        setAccount(data);
      } catch (err) {
        console.error('Error fetching account:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch account');
      } finally {
        setLoading(false);
      }
    };

    if (accountId) {
      fetchAccount();
    }
  }, [accountId]);

  const handleEdit = () => {
    router.push(`/accounts/${accountId}/edit`);
  };

  const handleBack = () => {
    router.push('/accounts');
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'long',
      day: 'numeric',
    });
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading account details...</p>
        </div>
      </div>
    );
  }

  if (error || !account) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error || 'Account not found'}</p>
          <Button onClick={handleBack}>Back to Accounts</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-900 mb-2">Account Details</h1>
            <p className="text-gray-600">View account information and details</p>
          </div>
          <div className="flex gap-3">
            <Button variant="secondary" onClick={handleBack}>
              Back to List
            </Button>
            <Button onClick={handleEdit}>
              Edit Account
            </Button>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Main Information Card */}
        <div className="lg:col-span-2 bg-white shadow-md rounded-lg overflow-hidden">
          <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Account Information</h2>
          </div>
          
          <div className="p-6">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* Account ID */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Account ID
                </label>
                <p className="text-lg font-semibold text-gray-900">{account.accountId}</p>
              </div>

              {/* Status */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Status
                </label>
                <span
                  className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${
                    account.activeStatus === 'Y'
                      ? 'bg-green-100 text-green-800'
                      : 'bg-red-100 text-red-800'
                  }`}
                >
                  {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                </span>
              </div>

              {/* Group ID */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Group ID
                </label>
                <p className="text-lg text-gray-900">{account.groupId}</p>
              </div>

              {/* Address Zip Code */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Address Zip Code
                </label>
                <p className="text-lg text-gray-900">{account.addressZipCode}</p>
              </div>
            </div>
          </div>
        </div>

        {/* Status Card */}
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Quick Stats</h2>
          </div>
          
          <div className="p-6 space-y-4">
            <div>
              <p className="text-sm text-gray-500 mb-1">Current Balance</p>
              <p className={`text-2xl font-bold ${
                account.currentBalance >= 0 ? 'text-green-600' : 'text-red-600'
              }`}>
                {formatCurrency(account.currentBalance)}
              </p>
            </div>
            
            <div className="pt-4 border-t border-gray-200">
              <p className="text-sm text-gray-500 mb-1">Available Credit</p>
              <p className="text-xl font-semibold text-gray-900">
                {formatCurrency(account.creditLimit - account.currentBalance)}
              </p>
            </div>

            <div className="pt-4 border-t border-gray-200">
              <p className="text-sm text-gray-500 mb-1">Credit Utilization</p>
              <div className="mt-2">
                <div className="flex items-center justify-between text-sm mb-1">
                  <span className="text-gray-600">
                    {((account.currentBalance / account.creditLimit) * 100).toFixed(1)}%
                  </span>
                  <span className="text-gray-500">
                    {formatCurrency(account.currentBalance)} / {formatCurrency(account.creditLimit)}
                  </span>
                </div>
                <div className="w-full bg-gray-200 rounded-full h-2">
                  <div
                    className={`h-2 rounded-full ${
                      (account.currentBalance / account.creditLimit) * 100 > 80
                        ? 'bg-red-500'
                        : (account.currentBalance / account.creditLimit) * 100 > 50
                        ? 'bg-yellow-500'
                        : 'bg-green-500'
                    }`}
                    style={{ width: `${Math.min((account.currentBalance / account.creditLimit) * 100, 100)}%` }}
                  ></div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Financial Details */}
      <div className="mt-6 bg-white shadow-md rounded-lg overflow-hidden">
        <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
          <h2 className="text-xl font-semibold text-gray-900">Financial Details</h2>
        </div>
        
        <div className="p-6">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
            {/* Current Balance */}
            <div className="bg-gray-50 rounded-lg p-4">
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Current Balance
              </label>
              <p className="text-2xl font-bold text-gray-900">
                {formatCurrency(account.currentBalance)}
              </p>
            </div>

            {/* Credit Limit */}
            <div className="bg-gray-50 rounded-lg p-4">
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Credit Limit
              </label>
              <p className="text-2xl font-bold text-gray-900">
                {formatCurrency(account.creditLimit)}
              </p>
            </div>

            {/* Cash Credit Limit */}
            <div className="bg-gray-50 rounded-lg p-4">
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Cash Credit Limit
              </label>
              <p className="text-2xl font-bold text-gray-900">
                {formatCurrency(account.cashCreditLimit)}
              </p>
            </div>

            {/* Available Credit */}
            <div className="bg-gray-50 rounded-lg p-4">
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Available Credit
              </label>
              <p className="text-2xl font-bold text-green-600">
                {formatCurrency(account.creditLimit - account.currentBalance)}
              </p>
            </div>
          </div>
        </div>
      </div>

      {/* Cycle Information */}
      <div className="mt-6 bg-white shadow-md rounded-lg overflow-hidden">
        <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
          <h2 className="text-xl font-semibold text-gray-900">Current Cycle Activity</h2>
        </div>
        
        <div className="p-6">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {/* Current Cycle Credit */}
            <div className="flex items-center justify-between p-4 bg-green-50 rounded-lg">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Cycle Credits
                </label>
                <p className="text-2xl font-bold text-green-700">
                  {formatCurrency(account.currentCycleCredit)}
                </p>
              </div>
              <div className="text-green-600">
                <svg className="w-12 h-12" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 4v16m8-8H4" />
                </svg>
              </div>
            </div>

            {/* Current Cycle Debit */}
            <div className="flex items-center justify-between p-4 bg-red-50 rounded-lg">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Cycle Debits
                </label>
                <p className="text-2xl font-bold text-red-700">
                  {formatCurrency(account.currentCycleDebit)}
                </p>
              </div>
              <div className="text-red-600">
                <svg className="w-12 h-12" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M20 12H4" />
                </svg>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Date Information */}
      <div className="mt-6 bg-white shadow-md rounded-lg overflow-hidden">
        <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
          <h2 className="text-xl font-semibold text-gray-900">Important Dates</h2>
        </div>
        
        <div className="p-6">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {/* Open Date */}
            <div>
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Account Open Date
              </label>
              <p className="text-lg text-gray-900">{formatDate(account.openDate)}</p>
            </div>

            {/* Expiration Date */}
            <div>
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Expiration Date
              </label>
              <p className="text-lg text-gray-900">{formatDate(account.expirationDate)}</p>
            </div>

            {/* Reissue Date */}
            <div>
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Reissue Date
              </label>
              <p className="text-lg text-gray-900">
                {account.reissueDate ? formatDate(account.reissueDate) : 'N/A'}
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AccountDetailPage;
