'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

interface DashboardStats {
  totalAccounts: number;
  totalCustomers: number;
  totalCards: number;
  totalTransactions: number;
  pendingPayments: number;
}

interface NavigationCard {
  title: string;
  description: string;
  icon: string;
  route: string;
  color: string;
  stats?: number;
}

export default function DashboardPage() {
  const router = useRouter();
  const [stats, setStats] = useState<DashboardStats>({
    totalAccounts: 0,
    totalCustomers: 0,
    totalCards: 0,
    totalTransactions: 0,
    pendingPayments: 0,
  });
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetchDashboardStats();
  }, []);

  const fetchDashboardStats = async () => {
    try {
      setLoading(true);
      // Simulated stats - replace with actual API calls when available
      const mockStats: DashboardStats = {
        totalAccounts: 1250,
        totalCustomers: 3420,
        totalCards: 4680,
        totalTransactions: 15340,
        pendingPayments: 87,
      };
      setStats(mockStats);
      setError(null);
    } catch (err) {
      setError('Failed to load dashboard statistics');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const navigationCards: NavigationCard[] = [
    {
      title: 'Accounts Management',
      description: 'Manage credit card accounts, balances, and credit limits',
      icon: '💳',
      route: '/accounts',
      color: 'bg-blue-500',
      stats: stats.totalAccounts,
    },
    {
      title: 'Customer Management',
      description: 'View and manage customer information and profiles',
      icon: '👥',
      route: '/customers',
      color: 'bg-green-500',
      stats: stats.totalCustomers,
    },
    {
      title: 'Card Management',
      description: 'Issue, activate, and manage credit cards',
      icon: '💎',
      route: '/cards',
      color: 'bg-purple-500',
      stats: stats.totalCards,
    },
    {
      title: 'Transaction View',
      description: 'View and analyze transaction history and details',
      icon: '📊',
      route: '/transactions',
      color: 'bg-orange-500',
      stats: stats.totalTransactions,
    },
    {
      title: 'Bill Payment Processing',
      description: 'Process and manage bill payments for accounts',
      icon: '💰',
      route: '/bill-payments',
      color: 'bg-red-500',
      stats: stats.pendingPayments,
    },
  ];

  const handleNavigate = (route: string) => {
    router.push(route);
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-16 w-16 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Loading dashboard...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex items-center">
            <span className="text-red-600 text-xl mr-3">⚠️</span>
            <div>
              <h3 className="text-red-800 font-semibold">Error Loading Dashboard</h3>
              <p className="text-red-600">{error}</p>
            </div>
          </div>
          <Button
            variant="secondary"
            onClick={fetchDashboardStats}
            className="mt-4"
          >
            Retry
          </Button>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900 mb-2">
            Card Services Dashboard
          </h1>
          <p className="text-gray-600">
            Welcome to the Card Services Management System
          </p>
        </div>

        {/* Summary Statistics */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-5 gap-6 mb-8">
          <div className="bg-white rounded-lg shadow p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Total Accounts</p>
                <p className="text-2xl font-bold text-gray-900 mt-1">
                  {stats.totalAccounts.toLocaleString()}
                </p>
              </div>
              <div className="text-3xl">💳</div>
            </div>
          </div>

          <div className="bg-white rounded-lg shadow p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Total Customers</p>
                <p className="text-2xl font-bold text-gray-900 mt-1">
                  {stats.totalCustomers.toLocaleString()}
                </p>
              </div>
              <div className="text-3xl">👥</div>
            </div>
          </div>

          <div className="bg-white rounded-lg shadow p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Active Cards</p>
                <p className="text-2xl font-bold text-gray-900 mt-1">
                  {stats.totalCards.toLocaleString()}
                </p>
              </div>
              <div className="text-3xl">💎</div>
            </div>
          </div>

          <div className="bg-white rounded-lg shadow p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Transactions</p>
                <p className="text-2xl font-bold text-gray-900 mt-1">
                  {stats.totalTransactions.toLocaleString()}
                </p>
              </div>
              <div className="text-3xl">📊</div>
            </div>
          </div>

          <div className="bg-white rounded-lg shadow p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Pending Payments</p>
                <p className="text-2xl font-bold text-gray-900 mt-1">
                  {stats.pendingPayments.toLocaleString()}
                </p>
              </div>
              <div className="text-3xl">💰</div>
            </div>
          </div>
        </div>

        {/* Navigation Cards */}
        <div className="mb-8">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">Quick Access</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {navigationCards.map((card, index) => (
              <div
                key={index}
                className="bg-white rounded-lg shadow hover:shadow-lg transition-shadow duration-200 cursor-pointer overflow-hidden"
                onClick={() => handleNavigate(card.route)}
              >
                <div className={`${card.color} h-2`}></div>
                <div className="p-6">
                  <div className="flex items-start justify-between mb-4">
                    <div className="text-4xl">{card.icon}</div>
                    {card.stats !== undefined && (
                      <div className="bg-gray-100 rounded-full px-3 py-1">
                        <span className="text-sm font-semibold text-gray-700">
                          {card.stats.toLocaleString()}
                        </span>
                      </div>
                    )}
                  </div>
                  <h3 className="text-lg font-semibold text-gray-900 mb-2">
                    {card.title}
                  </h3>
                  <p className="text-sm text-gray-600 mb-4">
                    {card.description}
                  </p>
                  <Button
                    variant="secondary"
                    size="sm"
                    onClick={(e) => {
                      e.stopPropagation();
                      handleNavigate(card.route);
                    }}
                  >
                    Open →
                  </Button>
                </div>
              </div>
            ))}
          </div>
        </div>

        {/* Recent Activity Section */}
        <div className="bg-white rounded-lg shadow p-6">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-xl font-semibold text-gray-900">Recent Activity</h2>
            <Button
              variant="secondary"
              size="sm"
              onClick={() => router.push('/transactions')}
            >
              View All
            </Button>
          </div>
          <div className="space-y-4">
            <div className="flex items-center justify-between py-3 border-b border-gray-200">
              <div className="flex items-center">
                <div className="w-10 h-10 bg-blue-100 rounded-full flex items-center justify-center mr-3">
                  <span className="text-blue-600 font-semibold">💳</span>
                </div>
                <div>
                  <p className="text-sm font-medium text-gray-900">New Account Created</p>
                  <p className="text-xs text-gray-500">Account #12345678901</p>
                </div>
              </div>
              <span className="text-xs text-gray-500">2 hours ago</span>
            </div>
            <div className="flex items-center justify-between py-3 border-b border-gray-200">
              <div className="flex items-center">
                <div className="w-10 h-10 bg-green-100 rounded-full flex items-center justify-center mr-3">
                  <span className="text-green-600 font-semibold">💰</span>
                </div>
                <div>
                  <p className="text-sm font-medium text-gray-900">Payment Processed</p>
                  <p className="text-xs text-gray-500">$1,250.00</p>
                </div>
              </div>
              <span className="text-xs text-gray-500">5 hours ago</span>
            </div>
            <div className="flex items-center justify-between py-3">
              <div className="flex items-center">
                <div className="w-10 h-10 bg-purple-100 rounded-full flex items-center justify-center mr-3">
                  <span className="text-purple-600 font-semibold">💎</span>
                </div>
                <div>
                  <p className="text-sm font-medium text-gray-900">Card Activated</p>
                  <p className="text-xs text-gray-500">Card ending in 4532</p>
                </div>
              </div>
              <span className="text-xs text-gray-500">1 day ago</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
