'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { cardService } from '@/services/cardService';
import { transactionService } from '@/services/transactionService';

export default function HomePage() {
  const router = useRouter();
  const [stats, setStats] = useState({
    totalAccounts: 0,
    totalCustomers: 0,
    totalCards: 0,
    totalTransactions: 0,
  });
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    fetchStats();
  }, []);

  const fetchStats = async () => {
    try {
      setLoading(true);
      const [accounts, customers, cards, transactions] = await Promise.all([
        accountService.getAccounts(),
        customerService.getCustomers(),
        cardService.getCards(),
        transactionService.getTransactions(),
      ]);

      setStats({
        totalAccounts: accounts.length,
        totalCustomers: customers.length,
        totalCards: cards.length,
        totalTransactions: transactions.length,
      });
    } catch (err) {
      console.error('Failed to load stats:', err);
    } finally {
      setLoading(false);
    }
  };

  const StatCard = ({ title, value, icon, color, onClick }: any) => (
    <div
      onClick={onClick}
      className={`bg-white shadow rounded-lg p-6 cursor-pointer hover:shadow-lg transition-shadow border-l-4 ${color}`}
    >
      <div className="flex items-center justify-between">
        <div>
          <p className="text-sm font-medium text-gray-600">{title}</p>
          <p className="text-3xl font-bold text-gray-900 mt-2">{value}</p>
        </div>
        <div className="text-4xl">{icon}</div>
      </div>
    </div>
  );

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Loading dashboard...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900">CardDemo Dashboard</h1>
        <p className="text-gray-600 mt-2">Account and Customer Data Processing System</p>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <StatCard
          title="Total Accounts"
          value={stats.totalAccounts}
          icon="🏦"
          color="border-blue-500"
          onClick={() => router.push('/accounts')}
        />
        <StatCard
          title="Total Customers"
          value={stats.totalCustomers}
          icon="👥"
          color="border-green-500"
          onClick={() => router.push('/customers')}
        />
        <StatCard
          title="Total Cards"
          value={stats.totalCards}
          icon="💳"
          color="border-purple-500"
          onClick={() => router.push('/cards')}
        />
        <StatCard
          title="Total Transactions"
          value={stats.totalTransactions}
          icon="📊"
          color="border-orange-500"
          onClick={() => router.push('/transactions')}
        />
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">Quick Actions</h2>
          <div className="space-y-3">
            <button
              onClick={() => router.push('/accounts/new')}
              className="w-full px-4 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 text-left flex items-center justify-between"
            >
              <span>Create New Account</span>
              <span>→</span>
            </button>
            <button
              onClick={() => router.push('/customers/new')}
              className="w-full px-4 py-3 bg-green-600 text-white rounded-lg hover:bg-green-700 text-left flex items-center justify-between"
            >
              <span>Create New Customer</span>
              <span>→</span>
            </button>
            <button
              onClick={() => router.push('/cards/new')}
              className="w-full px-4 py-3 bg-purple-600 text-white rounded-lg hover:bg-purple-700 text-left flex items-center justify-between"
            >
              <span>Create New Card</span>
              <span>→</span>
            </button>
            <button
              onClick={() => router.push('/transactions/new')}
              className="w-full px-4 py-3 bg-orange-600 text-white rounded-lg hover:bg-orange-700 text-left flex items-center justify-between"
            >
              <span>Create New Transaction</span>
              <span>→</span>
            </button>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold text-gray-900 mb-4">Reports & Tools</h2>
          <div className="space-y-3">
            <button
              onClick={() => router.push('/statements')}
              className="w-full px-4 py-3 bg-gray-100 text-gray-900 rounded-lg hover:bg-gray-200 text-left flex items-center justify-between"
            >
              <span>View Account Statements</span>
              <span>→</span>
            </button>
            <button
              onClick={() => router.push('/interest')}
              className="w-full px-4 py-3 bg-gray-100 text-gray-900 rounded-lg hover:bg-gray-200 text-left flex items-center justify-between"
            >
              <span>Calculate Interest</span>
              <span>→</span>
            </button>
            <button
              onClick={() => router.push('/accounts')}
              className="w-full px-4 py-3 bg-gray-100 text-gray-900 rounded-lg hover:bg-gray-200 text-left flex items-center justify-between"
            >
              <span>Browse All Accounts</span>
              <span>→</span>
            </button>
            <button
              onClick={() => router.push('/transactions')}
              className="w-full px-4 py-3 bg-gray-100 text-gray-900 rounded-lg hover:bg-gray-200 text-left flex items-center justify-between"
            >
              <span>Browse All Transactions</span>
              <span>→</span>
            </button>
          </div>
        </div>
      </div>

      <div className="mt-8 bg-blue-50 border border-blue-200 rounded-lg p-6">
        <h3 className="text-lg font-semibold text-blue-900 mb-2">Welcome to CardDemo</h3>
        <p className="text-blue-800">
          This is a comprehensive account and customer data processing system. Use the navigation above or the quick
          actions to manage accounts, customers, cards, and transactions. Generate statements and calculate interest
          for all accounts.
        </p>
      </div>
    </div>
  );
}
