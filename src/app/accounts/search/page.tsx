'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Input, Button } from '@/components/ui';

export default function AccountSearchPage() {
  const router = useRouter();
  const [accountId, setAccountId] = useState<string>('');
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const validateAccountId = (): boolean => {
    if (!accountId || accountId.trim() === '') {
      setError('Account ID is required');
      return false;
    }

    const accountIdNum = parseInt(accountId, 10);
    if (isNaN(accountIdNum) || accountIdNum <= 0) {
      setError('Account ID must be a valid positive number');
      return false;
    }

    return true;
  };

  const handleSearch = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);

    if (!validateAccountId()) {
      return;
    }

    try {
      setLoading(true);
      const accountIdNum = parseInt(accountId, 10);
      await accountService.getAccountById(accountIdNum);
      router.push(`/accounts/${accountIdNum}`);
    } catch (err) {
      if (err instanceof Error && err.message === 'Account not found') {
        setError('Account not found. Please verify the account ID and try again.');
      } else {
        setError('Failed to search for account. Please try again.');
      }
      console.error('Error searching account:', err);
      setAccountId('');
    } finally {
      setLoading(false);
    }
  };

  const handleExit = () => {
    router.push('/');
  };

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-2xl mx-auto">
        <div className="bg-white shadow rounded-lg p-6">
          <div className="mb-6">
            <h1 className="text-2xl font-bold text-gray-900">Account Search</h1>
            <p className="text-sm text-gray-600 mt-1">
              Enter an account ID to view or edit account details
            </p>
          </div>

          {error && (
            <div className="mb-4 p-4 bg-red-50 border border-red-200 rounded-md">
              <p className="text-sm text-red-800">{error}</p>
            </div>
          )}

          <form onSubmit={handleSearch} className="space-y-6">
            <div>
              <Input
                label="Account ID"
                type="text"
                value={accountId}
                onChange={(e) => {
                  setAccountId(e.target.value);
                  setError(null);
                }}
                placeholder="Enter account ID"
                required
                autoFocus
              />
              <p className="mt-1 text-xs text-gray-500">
                Enter the 11-digit account identifier
              </p>
            </div>

            <div className="flex gap-3 pt-4">
              <Button
                type="submit"
                disabled={loading || !accountId}
              >
                {loading ? 'Searching...' : 'Search'}
              </Button>
              <Button
                type="button"
                variant="secondary"
                onClick={handleExit}
              >
                Exit
              </Button>
            </div>
          </form>
        </div>

        <div className="mt-6 bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h2 className="text-sm font-semibold text-blue-900 mb-2">Quick Actions</h2>
          <div className="space-y-2">
            <button
              onClick={() => router.push('/accounts')}
              className="text-sm text-blue-700 hover:text-blue-900 underline block"
            >
              View all accounts
            </button>
            <button
              onClick={() => router.push('/accounts/new')}
              className="text-sm text-blue-700 hover:text-blue-900 underline block"
            >
              Create new account
            </button>
          </div>
        </div>
      </div>
    </div>
  );
}
