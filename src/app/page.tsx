'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import { transactionService } from '@/services/transactionService';
import { accountService } from '@/services/accountService';
import { cardService } from '@/services/cardService';

interface DashboardStats {
  totalTransactions: number;
  totalAccounts: number;
  totalCards: number;
}

interface NavigationCard {
  title: string;
  description: string;
  icon: string;
  path: string;
  color: string;
}

export default function DashboardPage() {
  const router = useRouter();
  const [stats, setStats] = useState<DashboardStats>({
    totalTransactions: 0,
    totalAccounts: 0,
    totalCards: 0,
  });
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const navigationCards: NavigationCard[] = [
    {
      title: 'Transactions',
      description: 'View and manage card transactions',
      icon: '💳',
      path: '/transactions',
      color: 'bg-blue-50 border-blue-200 hover:bg-blue-100',
    },
    {
      title: 'Accounts',
      description: 'Manage customer accounts',
      icon: '👤',
      path: '/accounts',
      color: 'bg-green-50 border-green-200 hover:bg-green-100',
    },
    {
      title: 'Cards',
      description: 'View and manage cards',
      icon: '🎴',
      path: '/cards',
      color: 'bg-purple-50 border-purple-200 hover:bg-purple-100',
    },
    {
      title: 'Card Cross References',
      description: 'Manage card-account relationships',
      icon: '🔗',
      path: '/card-cross-references',
      color: 'bg-orange-50 border-orange-200 hover:bg-orange-100',
    },
  ];

  useEffect(() => {
    fetchDashboardStats();
  }, []);

  const fetchDashboardStats = async () => {
    try {
      setLoading(true);
      setError(null);

      const [transactionsRes, accountsRes, cardsRes] = await Promise.allSettled([
        transactionService.getTransactions(0, 1),
        accountService.getAccounts(0, 1),
        cardService.getCards(0, 1),
      ]);

      setStats({
        totalTransactions: transactionsRes.status === 'fulfilled' ? transactionsRes.value.totalElements : 0,
        totalAccounts: accountsRes.status === 'fulfilled' ? accountsRes.value.totalElements : 0,
        totalCards: cardsRes.status === 'fulfilled' ? cardsRes.value.totalElements : 0,
      });
    } catch (err) {
      console.error('Error fetching dashboard stats:', err);
      setError('Failed to load dashboard statistics');
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
          <p className="mt-4 text-gray-600">Loading dashboard...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-7xl mx-auto">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">
          Card Transaction Lifecycle Management
        </h1>
        <p className="text-gray-600">
          Welcome to your dashboard. Manage transactions, accounts, cards, and cross-references.
        </p>
      </div>

      {error && (
        <div className="mb-6 bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                <path
                  fillRule="evenodd"
                  d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
                  clipRule="evenodd"
                />
              </svg>
            </div>
            <div className="ml-3">
              <p className="text-sm text-red-700">{error}</p>
            </div>
          </div>
        </div>
      )}

      <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
        <div className="bg-white rounded-lg shadow p-6 border border-gray-200">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm font-medium text-gray-600 mb-1">Total Transactions</p>
              <p className="text-3xl font-bold text-gray-900">{stats.totalTransactions}</p>
            </div>
            <div className="bg-blue-100 rounded-full p-3">
              <svg
                className="h-8 w-8 text-blue-600"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z"
                />
              </svg>
            </div>
          </div>
        </div>

        <div className="bg-white rounded-lg shadow p-6 border border-gray-200">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm font-medium text-gray-600 mb-1">Total Accounts</p>
              <p className="text-3xl font-bold text-gray-900">{stats.totalAccounts}</p>
            </div>
            <div className="bg-green-100 rounded-full p-3">
              <svg
                className="h-8 w-8 text-green-600"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"
                />
              </svg>
            </div>
          </div>
        </div>

        <div className="bg-white rounded-lg shadow p-6 border border-gray-200">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-sm font-medium text-gray-600 mb-1">Total Cards</p>
              <p className="text-3xl font-bold text-gray-900">{stats.totalCards}</p>
            </div>
            <div className="bg-purple-100 rounded-full p-3">
              <svg
                className="h-8 w-8 text-purple-600"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
                />
              </svg>
            </div>
          </div>
        </div>
      </div>

      <div className="mb-8">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-2xl font-bold text-gray-900">Quick Actions</h2>
        </div>
        <div className="bg-white rounded-lg shadow p-6 border border-gray-200">
          <div className="flex flex-wrap gap-4">
            <Button onClick={() => router.push('/transactions/new')}>Create New Transaction</Button>
            <Button variant="secondary" onClick={() => router.push('/accounts/new')}>
              Create New Account
            </Button>
            <Button variant="secondary" onClick={() => router.push('/cards/new')}>
              Create New Card
            </Button>
            <Button variant="secondary" onClick={() => router.push('/card-cross-references/new')}>
              Create Card Cross Reference
            </Button>
          </div>
        </div>
      </div>

      <div>
        <h2 className="text-2xl font-bold text-gray-900 mb-4">Navigation</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          {navigationCards.map((card) => (
            <button
              key={card.path}
              onClick={() => router.push(card.path)}
              className={`${card.color} rounded-lg border-2 p-6 text-left transition-all duration-200 transform hover:scale-105 hover:shadow-lg focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500`}
            >
              <div className="text-4xl mb-3">{card.icon}</div>
              <h3 className="text-lg font-semibold text-gray-900 mb-2">{card.title}</h3>
              <p className="text-sm text-gray-600">{card.description}</p>
            </button>
          ))}
        </div>
      </div>

      <div className="mt-8 bg-blue-50 border border-blue-200 rounded-lg p-6">
        <div className="flex items-start">
          <div className="flex-shrink-0">
            <svg
              className="h-6 w-6 text-blue-600"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
          </div>
          <div className="ml-3">
            <h3 className="text-sm font-medium text-blue-800">System Information</h3>
            <p className="mt-2 text-sm text-blue-700">
              This dashboard provides an overview of your Card Transaction Lifecycle Management system.
              Use the navigation cards above to access different sections of the application.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
