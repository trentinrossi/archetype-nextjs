'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Account, Card, Transaction } from '@/types/cardServices';
import { accountService } from '@/services/accountService';
import { cardService } from '@/services/cardService';
import { transactionService } from '@/services/transactionService';
import { Button, Table } from '@/components/ui';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [cards, setCards] = useState<Card[]>([]);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [cardsLoading, setCardsLoading] = useState(true);
  const [transactionsLoading, setTransactionsLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.accountId) {
      fetchAccount(params.accountId as string);
      fetchCards(params.accountId as string);
    }
  }, [params.accountId]);

  useEffect(() => {
    if (cards.length > 0) {
      fetchTransactions();
    } else {
      setTransactionsLoading(false);
    }
  }, [cards]);

  const fetchAccount = async (accountId: string) => {
    try {
      setLoading(true);
      const data = await accountService.getAccountById(accountId);
      setAccount(data);
      setError(null);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load account details');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const fetchCards = async (accountId: string) => {
    try {
      setCardsLoading(true);
      const data = await cardService.getCardsByAccountId(accountId);
      setCards(data);
    } catch (err) {
      console.error('Failed to load cards:', err);
    } finally {
      setCardsLoading(false);
    }
  };

  const fetchTransactions = async () => {
    try {
      setTransactionsLoading(true);
      const allTransactions: Transaction[] = [];
      
      for (const card of cards) {
        try {
          const data = await transactionService.getTransactionsByCardNumber(card.cardNumber);
          allTransactions.push(...data);
        } catch (err) {
          console.error(`Failed to load transactions for card ${card.cardNumber}:`, err);
        }
      }
      
      allTransactions.sort((a, b) => 
        new Date(b.processingTimestamp).getTime() - new Date(a.processingTimestamp).getTime()
      );
      
      setTransactions(allTransactions.slice(0, 10));
    } catch (err) {
      console.error('Failed to load transactions:', err);
    } finally {
      setTransactionsLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this account? This action cannot be undone.')) {
      return;
    }

    try {
      await accountService.deleteAccount(params.accountId as string);
      router.push('/accounts');
    } catch (err) {
      alert(err instanceof Error ? err.message : 'Failed to delete account');
      console.error(err);
    }
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string): string => {
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const formatDateTime = (dateString: string): string => {
    return new Date(dateString).toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  const maskCardNumber = (cardNumber: string): string => {
    return `**** **** **** ${cardNumber.slice(-4)}`;
  };

  const getUtilizationPercentage = (): number => {
    if (!account || account.creditLimit === 0) return 0;
    return (account.currentBalance / account.creditLimit) * 100;
  };

  const getUtilizationColor = (): string => {
    const percentage = getUtilizationPercentage();
    if (percentage >= 90) return 'bg-red-500';
    if (percentage >= 70) return 'bg-yellow-500';
    return 'bg-green-500';
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-16 w-16 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Loading account details...</p>
        </div>
      </div>
    );
  }

  if (error || !account) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex items-center">
            <span className="text-red-600 text-xl mr-3">⚠️</span>
            <div>
              <h3 className="text-red-800 font-semibold">Error Loading Account</h3>
              <p className="text-red-600">{error || 'Account not found'}</p>
            </div>
          </div>
          <div className="mt-4 flex gap-2">
            <Button variant="secondary" onClick={() => router.push('/accounts')}>
              Back to Accounts
            </Button>
            <Button onClick={() => fetchAccount(params.accountId as string)}>
              Retry
            </Button>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Account Details</h1>
          <p className="text-sm text-gray-600 mt-1 font-mono">{account.accountId}</p>
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

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6 mb-6">
        <div className="bg-white rounded-lg shadow p-6">
          <div className="flex items-center justify-between mb-2">
            <h3 className="text-sm font-medium text-gray-500">Account Status</h3>
            <span
              className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                account.activeStatus === 'Y'
                  ? 'bg-green-100 text-green-800'
                  : 'bg-red-100 text-red-800'
              }`}
            >
              {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
            </span>
          </div>
          {account.expired && (
            <div className="mt-2">
              <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-red-100 text-red-800">
                Expired
              </span>
            </div>
          )}
        </div>

        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-sm font-medium text-gray-500 mb-2">Current Balance</h3>
          <p className="text-3xl font-bold text-gray-900">{formatCurrency(account.currentBalance)}</p>
        </div>

        <div className="bg-white rounded-lg shadow p-6">
          <h3 className="text-sm font-medium text-gray-500 mb-2">Available Credit</h3>
          <p className="text-3xl font-bold text-green-600">{formatCurrency(account.availableCredit)}</p>
        </div>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Credit Information</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Credit Limit</label>
            <p className="text-lg font-semibold text-gray-900">{formatCurrency(account.creditLimit)}</p>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Cash Credit Limit</label>
            <p className="text-lg font-semibold text-gray-900">{formatCurrency(account.cashCreditLimit)}</p>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Credit Utilization</label>
            <div className="flex items-center gap-2">
              <div className="flex-1 bg-gray-200 rounded-full h-2">
                <div
                  className={`h-2 rounded-full ${getUtilizationColor()}`}
                  style={{ width: `${Math.min(getUtilizationPercentage(), 100)}%` }}
                ></div>
              </div>
              <span className="text-sm font-semibold text-gray-900">
                {getUtilizationPercentage().toFixed(1)}%
              </span>
            </div>
          </div>
        </div>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Account Dates</h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Open Date</label>
            <p className="text-gray-900">{formatDate(account.openDate)}</p>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Expiration Date</label>
            <p className={`${account.expired ? 'text-red-600 font-semibold' : 'text-gray-900'}`}>
              {formatDate(account.expirationDate)}
              {account.expired && ' (Expired)'}
            </p>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Reissue Date</label>
            <p className="text-gray-900">{formatDate(account.reissueDate)}</p>
          </div>
        </div>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Current Cycle Information</h2>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Group ID</label>
            <p className="text-gray-900 font-mono">{account.groupId}</p>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Current Cycle Credit</label>
            <p className="text-lg font-semibold text-green-600">{formatCurrency(account.currentCycleCredit)}</p>
          </div>
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-1">Current Cycle Debit</label>
            <p className="text-lg font-semibold text-red-600">{formatCurrency(account.currentCycleDebit)}</p>
          </div>
        </div>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-lg font-semibold text-gray-900">Associated Cards</h2>
          {cards.length > 0 && (
            <span className="text-sm text-gray-600">{cards.length} card(s)</span>
          )}
        </div>
        {cardsLoading ? (
          <div className="text-center py-8">
            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-2"></div>
            <p className="text-sm text-gray-600">Loading cards...</p>
          </div>
        ) : cards.length === 0 ? (
          <div className="text-center py-8 text-gray-500">
            <p>No cards associated with this account</p>
          </div>
        ) : (
          <div className="overflow-x-auto">
            <Table
              columns={[
                {
                  key: 'cardNumber',
                  label: 'Card Number',
                  render: (card: Card) => (
                    <span className="font-mono text-sm">{maskCardNumber(card.cardNumber)}</span>
                  ),
                },
                {
                  key: 'embossedName',
                  label: 'Cardholder Name',
                },
                {
                  key: 'expirationDate',
                  label: 'Expiration Date',
                  render: (card: Card) => (
                    <span className={card.expired ? 'text-red-600 font-semibold' : ''}>
                      {formatDate(card.expirationDate)}
                      {card.expired && ' (Expired)'}
                    </span>
                  ),
                },
                {
                  key: 'activeStatus',
                  label: 'Status',
                  render: (card: Card) => (
                    <span
                      className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                        card.activeStatus === 'Y'
                          ? 'bg-green-100 text-green-800'
                          : 'bg-red-100 text-red-800'
                      }`}
                    >
                      {card.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                    </span>
                  ),
                },
              ]}
              data={cards}
              onRowClick={(card) => router.push(`/cards/${card.cardNumber}`)}
              actions={(card) => (
                <Button
                  size="sm"
                  onClick={(e) => {
                    e.stopPropagation();
                    router.push(`/cards/${card.cardNumber}`);
                  }}
                >
                  View Details
                </Button>
              )}
            />
          </div>
        )}
      </div>

      <div className="bg-white rounded-lg shadow p-6">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-lg font-semibold text-gray-900">Recent Transactions</h2>
          {transactions.length > 0 && (
            <span className="text-sm text-gray-600">Last 10 transactions</span>
          )}
        </div>
        {transactionsLoading ? (
          <div className="text-center py-8">
            <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-2"></div>
            <p className="text-sm text-gray-600">Loading transactions...</p>
          </div>
        ) : transactions.length === 0 ? (
          <div className="text-center py-8 text-gray-500">
            <p>No recent transactions found</p>
          </div>
        ) : (
          <div className="overflow-x-auto">
            <Table
              columns={[
                {
                  key: 'processingTimestamp',
                  label: 'Date',
                  render: (transaction: Transaction) => (
                    <span className="text-sm">{formatDateTime(transaction.processingTimestamp)}</span>
                  ),
                },
                {
                  key: 'description',
                  label: 'Description',
                  render: (transaction: Transaction) => (
                    <div>
                      <p className="font-medium">{transaction.description}</p>
                      <p className="text-xs text-gray-500">{transaction.merchantName}</p>
                    </div>
                  ),
                },
                {
                  key: 'categoryCode',
                  label: 'Category',
                  render: (transaction: Transaction) => (
                    <span className="text-sm">{transaction.categoryCode}</span>
                  ),
                },
                {
                  key: 'cardNumber',
                  label: 'Card',
                  render: (transaction: Transaction) => (
                    <span className="font-mono text-xs">{maskCardNumber(transaction.cardNumber)}</span>
                  ),
                },
                {
                  key: 'amount',
                  label: 'Amount',
                  render: (transaction: Transaction) => (
                    <span className={`font-semibold ${transaction.amount < 0 ? 'text-red-600' : 'text-green-600'}`}>
                      {formatCurrency(Math.abs(transaction.amount))}
                    </span>
                  ),
                },
              ]}
              data={transactions}
              onRowClick={(transaction) => router.push(`/transactions/${transaction.transactionId}`)}
            />
          </div>
        )}
      </div>
    </div>
  );
}
