'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Account, Card, Transaction } from '@/types/cardDemo';
import { accountService } from '@/services/accountService';
import { cardService } from '@/services/cardService';
import { transactionService } from '@/services/transactionService';

type TabType = 'overview' | 'cards' | 'transactions';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [cards, setCards] = useState<Card[]>([]);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [activeTab, setActiveTab] = useState<TabType>('overview');
  const [loading, setLoading] = useState(true);
  const [cardsLoading, setCardsLoading] = useState(false);
  const [transactionsLoading, setTransactionsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.accountId) {
      fetchAccount(params.accountId as string);
    }
  }, [params.accountId]);

  useEffect(() => {
    if (account && activeTab === 'cards' && cards.length === 0) {
      fetchCards(account.accountId);
    }
  }, [activeTab, account]);

  useEffect(() => {
    if (account && activeTab === 'transactions' && transactions.length === 0) {
      fetchTransactions(account.accountId);
    }
  }, [activeTab, account]);

  const fetchAccount = async (accountId: string) => {
    try {
      setLoading(true);
      setError(null);
      const data = await accountService.getAccountById(accountId);
      setAccount(data);
    } catch (err) {
      setError('Failed to load account');
      console.error('Failed to load account:', err);
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

  const fetchTransactions = async (accountId: string) => {
    try {
      setTransactionsLoading(true);
      const data = await transactionService.getTransactionsByAccountId(accountId);
      setTransactions(data);
    } catch (err) {
      console.error('Failed to load transactions:', err);
    } finally {
      setTransactionsLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this account? This action cannot be undone.')) return;

    try {
      await accountService.deleteAccount(params.accountId as string);
      router.push('/accounts');
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const formatCurrency = (value: string) => {
    const num = parseFloat(value);
    return isNaN(num) ? value : `$${num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  };

  const formatDate = (dateString: string) => {
    try {
      return new Date(dateString).toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
      });
    } catch {
      return dateString;
    }
  };

  const getStatusBadgeColor = (status: string) => {
    switch (status.toUpperCase()) {
      case 'ACTIVE':
        return 'bg-green-100 text-green-800';
      case 'INACTIVE':
        return 'bg-gray-100 text-gray-800';
      case 'SUSPENDED':
        return 'bg-yellow-100 text-yellow-800';
      case 'CLOSED':
        return 'bg-red-100 text-red-800';
      case 'BLOCKED':
        return 'bg-red-100 text-red-800';
      case 'EXPIRED':
        return 'bg-orange-100 text-orange-800';
      default:
        return 'bg-blue-100 text-blue-800';
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
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
            <svg
              className="h-5 w-5 text-red-400 mr-2"
              fill="none"
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="2"
              viewBox="0 0 24 24"
              stroke="currentColor"
            >
              <path d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>
            </svg>
            <p className="text-red-800 font-medium">{error || 'Account not found'}</p>
          </div>
          <button
            onClick={() => router.push('/accounts')}
            className="mt-4 px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700"
          >
            Back to Accounts
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Account Details</h1>
          <p className="text-gray-600 mt-1">Account ID: {account.accountId}</p>
        </div>
        <div className="flex gap-2">
          <button
            onClick={() => router.push(`/accounts/${account.accountId}/edit`)}
            className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
          >
            Edit Account
          </button>
          <button
            onClick={handleDelete}
            className="px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700"
          >
            Delete Account
          </button>
          <button
            onClick={() => router.push('/accounts')}
            className="px-4 py-2 bg-gray-200 text-gray-700 rounded hover:bg-gray-300"
          >
            Back to List
          </button>
        </div>
      </div>

      <div className="border-b border-gray-200 mb-6">
        <nav className="-mb-px flex space-x-8">
          <button
            onClick={() => setActiveTab('overview')}
            className={`${
              activeTab === 'overview'
                ? 'border-blue-500 text-blue-600'
                : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
            } whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm`}
          >
            Overview
          </button>
          <button
            onClick={() => setActiveTab('cards')}
            className={`${
              activeTab === 'cards'
                ? 'border-blue-500 text-blue-600'
                : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
            } whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm`}
          >
            Cards ({cards.length})
          </button>
          <button
            onClick={() => setActiveTab('transactions')}
            className={`${
              activeTab === 'transactions'
                ? 'border-blue-500 text-blue-600'
                : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
            } whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm`}
          >
            Transactions ({transactions.length})
          </button>
        </nav>
      </div>

      {activeTab === 'overview' && (
        <div className="space-y-6">
          <div className="bg-white shadow rounded-lg p-6">
            <h2 className="text-lg font-semibold mb-4">Account Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Account ID</label>
                <p className="text-gray-900">{account.accountId}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Status</label>
                <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getStatusBadgeColor(account.activeStatus)}`}>
                  {account.activeStatus}
                </span>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Group ID</label>
                <p className="text-gray-900">{account.groupId}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Current Balance</label>
                <p className="text-gray-900 font-semibold text-lg">{formatCurrency(account.currentBalance)}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Credit Limit</label>
                <p className="text-gray-900 font-semibold text-lg">{formatCurrency(account.creditLimit)}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Cash Credit Limit</label>
                <p className="text-gray-900 font-semibold text-lg">{formatCurrency(account.cashCreditLimit)}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Available Credit</label>
                <p className="text-gray-900 font-semibold text-lg text-green-600">
                  {formatCurrency((parseFloat(account.creditLimit) - parseFloat(account.currentBalance)).toString())}
                </p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Current Cycle Credit</label>
                <p className="text-gray-900">{formatCurrency(account.currentCycleCredit)}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Current Cycle Debit</label>
                <p className="text-gray-900">{formatCurrency(account.currentCycleDebit)}</p>
              </div>
            </div>
          </div>

          <div className="bg-white shadow rounded-lg p-6">
            <h2 className="text-lg font-semibold mb-4">Dates</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Open Date</label>
                <p className="text-gray-900">{formatDate(account.openDate)}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Expiration Date</label>
                <p className="text-gray-900">{formatDate(account.expirationDate)}</p>
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Reissue Date</label>
                <p className="text-gray-900">{formatDate(account.reissueDate)}</p>
              </div>
            </div>
          </div>

          <div className="bg-white shadow rounded-lg p-6">
            <h2 className="text-lg font-semibold mb-4">Address Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-1">Zip Code</label>
                <p className="text-gray-900">{account.addressZipCode}</p>
              </div>
            </div>
          </div>
        </div>
      )}

      {activeTab === 'cards' && (
        <div className="bg-white shadow rounded-lg p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-lg font-semibold">Associated Cards</h2>
            <button
              onClick={() => router.push(`/cards/new?accountId=${account.accountId}`)}
              className="px-3 py-1.5 bg-blue-600 text-white text-sm rounded hover:bg-blue-700"
            >
              Add Card
            </button>
          </div>

          {cardsLoading ? (
            <div className="text-center py-8">
              <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-2"></div>
              <p className="text-gray-600">Loading cards...</p>
            </div>
          ) : cards.length === 0 ? (
            <div className="text-center py-8">
              <svg
                className="mx-auto h-12 w-12 text-gray-400 mb-4"
                fill="none"
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth="2"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path d="M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z"></path>
              </svg>
              <p className="text-gray-600 mb-4">No cards associated with this account</p>
              <button
                onClick={() => router.push(`/cards/new?accountId=${account.accountId}`)}
                className="px-3 py-1.5 bg-blue-600 text-white text-sm rounded hover:bg-blue-700"
              >
                Add First Card
              </button>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Card Number</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Embossed Name</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Status</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Expiration Date</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {cards.map((card) => (
                    <tr key={card.cardNumber} className="hover:bg-gray-50">
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                        {card.cardNumber.replace(/(\d{4})(?=\d)/g, '$1 ')}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">{card.embossedName}</td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getStatusBadgeColor(card.activeStatus)}`}>
                          {card.activeStatus}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">{formatDate(card.expirationDate)}</td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm">
                        <div className="flex gap-2">
                          <button
                            onClick={() => router.push(`/cards/${card.cardNumber}`)}
                            className="text-blue-600 hover:text-blue-900"
                          >
                            View
                          </button>
                          <button
                            onClick={() => router.push(`/cards/${card.cardNumber}/edit`)}
                            className="text-indigo-600 hover:text-indigo-900"
                          >
                            Edit
                          </button>
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      )}

      {activeTab === 'transactions' && (
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Recent Transactions</h2>

          {transactionsLoading ? (
            <div className="text-center py-8">
              <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-2"></div>
              <p className="text-gray-600">Loading transactions...</p>
            </div>
          ) : transactions.length === 0 ? (
            <div className="text-center py-8">
              <svg
                className="mx-auto h-12 w-12 text-gray-400 mb-4"
                fill="none"
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth="2"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"></path>
              </svg>
              <p className="text-gray-600">No transactions found for this account</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Date</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Description</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Merchant</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Type</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Category</th>
                    <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">Amount</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">Actions</th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {transactions.map((transaction) => (
                    <tr key={transaction.transactionId} className="hover:bg-gray-50">
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {formatDate(transaction.originalTimestamp)}
                      </td>
                      <td className="px-6 py-4 text-sm text-gray-900">{transaction.transactionDescription}</td>
                      <td className="px-6 py-4 text-sm text-gray-900">
                        <div>{transaction.merchantName}</div>
                        <div className="text-xs text-gray-500">{transaction.merchantCity}</div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">{transaction.transactionTypeCode}</td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">{transaction.transactionCategoryCode}</td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-right font-medium">
                        <span className={parseFloat(transaction.transactionAmount) < 0 ? 'text-red-600' : 'text-green-600'}>
                          {formatCurrency(transaction.transactionAmount)}
                        </span>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm">
                        <button
                          onClick={() => router.push(`/transactions/${transaction.transactionId}`)}
                          className="text-blue-600 hover:text-blue-900"
                        >
                          View
                        </button>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
