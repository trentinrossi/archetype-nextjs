'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { cardService } from '@/services/cardService';
import { Account } from '@/types/account';
import { Card } from '@/types/card';
import { Button, Table } from '@/components/ui';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [cards, setCards] = useState<Card[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.accountId) {
      fetchAccountDetails(params.accountId as string);
    }
  }, [params.accountId]);

  const fetchAccountDetails = async (accountId: string) => {
    try {
      setLoading(true);
      const [accountData, cardsData] = await Promise.all([
        accountService.getAccountById(accountId),
        cardService.getCardsByAccount(accountId).catch(() => []),
      ]);
      setAccount(accountData);
      setCards(cardsData);
      setError(null);
    } catch (err) {
      setError('Failed to load account details');
      console.error('Failed to load account:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this account? This will also delete all associated cards.')) return;

    try {
      await accountService.deleteAccount(params.accountId as string);
      router.push('/accounts');
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string) => {
    try {
      return new Date(dateString).toLocaleDateString();
    } catch {
      return dateString;
    }
  };

  if (loading) {
    return <div className="p-6">Loading...</div>;
  }

  if (error || !account) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded mb-4">
          {error || 'Account not found'}
        </div>
        <Button variant="secondary" onClick={() => router.push('/accounts')}>
          Back to Accounts
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6">
      {/* Header Section */}
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Account Details</h1>
          <p className="text-sm text-gray-600">COACTVWC - Account View</p>
        </div>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/accounts/${account.accountId}/edit`)}>
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

      {/* Account Information Section */}
      <div className="bg-white shadow rounded-lg p-6 space-y-6 mb-6">
        <h2 className="text-lg font-semibold border-b pb-2">Account Information</h2>
        
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {/* Account ID */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Account ID
            </label>
            <p className="text-gray-900 text-lg font-mono">{account.accountId}</p>
          </div>

          {/* Status */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Status
            </label>
            <div className="flex items-center gap-2">
              <span
                className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${
                  account.active
                    ? 'bg-green-100 text-green-800'
                    : 'bg-red-100 text-red-800'
                }`}
              >
                {account.active ? 'Active' : 'Inactive'}
              </span>
              {account.expired && (
                <span className="inline-flex items-center px-3 py-1 rounded-full text-sm font-medium bg-yellow-100 text-yellow-800">
                  Expired
                </span>
              )}
            </div>
          </div>

          {/* Current Balance */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Current Balance
            </label>
            <p className="text-gray-900 text-lg font-semibold">
              {formatCurrency(account.currentBalance)}
            </p>
          </div>

          {/* Credit Limit */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Credit Limit
            </label>
            <p className="text-gray-900">{formatCurrency(account.creditLimit)}</p>
          </div>

          {/* Available Credit */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Available Credit
            </label>
            <p className="text-gray-900 text-lg font-semibold text-green-600">
              {formatCurrency(account.availableCredit)}
            </p>
          </div>

          {/* Cash Credit Limit */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Cash Credit Limit
            </label>
            <p className="text-gray-900">{formatCurrency(account.cashCreditLimit)}</p>
          </div>

          {/* Open Date */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Open Date
            </label>
            <p className="text-gray-900">{formatDate(account.openDate)}</p>
          </div>

          {/* Expiration Date */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Expiration Date
            </label>
            <p className="text-gray-900">{formatDate(account.expirationDate)}</p>
          </div>

          {/* Reissue Date */}
          {account.reissueDate && (
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Reissue Date
              </label>
              <p className="text-gray-900">{formatDate(account.reissueDate)}</p>
            </div>
          )}

          {/* Current Cycle Credit */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Current Cycle Credit
            </label>
            <p className="text-gray-900">{formatCurrency(account.currentCycleCredit)}</p>
          </div>

          {/* Current Cycle Debit */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">
              Current Cycle Debit
            </label>
            <p className="text-gray-900">{formatCurrency(account.currentCycleDebit)}</p>
          </div>

          {/* Group ID */}
          {account.groupId && (
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Group ID
              </label>
              <p className="text-gray-900 font-mono">{account.groupId}</p>
            </div>
          )}
        </div>

        {/* Warnings */}
        {account.expired && (
          <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ This account has expired</p>
            <p className="text-sm mt-1">
              The account expired on {formatDate(account.expirationDate)}.
            </p>
          </div>
        )}

        {!account.active && !account.expired && (
          <div className="bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded">
            <p className="font-semibold">⚠️ This account is inactive</p>
            <p className="text-sm mt-1">
              The account is currently inactive and cannot be used.
            </p>
          </div>
        )}
      </div>

      {/* Associated Cards Section */}
      <div className="bg-white shadow rounded-lg p-6">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-lg font-semibold">Associated Cards</h2>
          <Button
            size="sm"
            onClick={() => router.push(`/cards/new?accountId=${account.accountId}`)}
          >
            Add Card
          </Button>
        </div>

        {cards.length > 0 ? (
          <Table
            columns={[
              { key: 'cardNumber', label: 'Card Number' },
              { key: 'embossedName', label: 'Embossed Name' },
              { key: 'expirationDate', label: 'Expiration Date' },
              { key: 'activeStatus', label: 'Status' },
            ]}
            data={cards.map(card => ({
              ...card,
              expirationDate: formatDate(card.expirationDate),
              activeStatus: card.active ? 'Active' : 'Inactive',
            }))}
            onRowClick={(card) => router.push(`/cards/${card.cardNumber}`)}
            actions={(card) => (
              <div className="flex gap-2">
                <Button
                  size="sm"
                  onClick={(e) => {
                    e.stopPropagation();
                    router.push(`/cards/${card.cardNumber}`);
                  }}
                >
                  View
                </Button>
                <Button
                  size="sm"
                  onClick={(e) => {
                    e.stopPropagation();
                    router.push(`/cards/${card.cardNumber}/edit`);
                  }}
                >
                  Edit
                </Button>
              </div>
            )}
          />
        ) : (
          <div className="text-center py-8 text-gray-500">
            No cards associated with this account
          </div>
        )}
      </div>
    </div>
  );
}
