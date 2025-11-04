'use client';

import React, { useEffect, useState } from 'react';
import { useRouter, useParams } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/transaction';
import { Button } from '@/components/ui';

export default function TransactionDetailPage() {
  const router = useRouter();
  const params = useParams();
  const transactionId = params.transactionId as string;
  const { isAuthenticated } = useAuth();
  const [transaction, setTransaction] = useState<Transaction | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [currentDateTime, setCurrentDateTime] = useState('');

  useEffect(() => {
    if (!isAuthenticated) {
      router.push('/login');
      return;
    }
  }, [isAuthenticated, router]);

  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const date = now.toLocaleDateString('en-US', { month: '2-digit', day: '2-digit', year: '2-digit' });
      const time = now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false });
      setCurrentDateTime(`${date} ${time}`);
    };
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    if (transactionId) {
      fetchTransaction();
    }
  }, [transactionId]);

  const fetchTransaction = async () => {
    try {
      setLoading(true);
      setError('');
      const data = await transactionService.getTransactionById(transactionId);
      setTransaction(data);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Transaction not found');
    } finally {
      setLoading(false);
    }
  };

  const formatAmount = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-green-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading transaction...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-green-600 to-teal-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CT01 - COTRN01C</h1>
              <p className="text-green-100 text-sm">Transaction Detail</p>
            </div>
            <p className="text-sm">{currentDateTime}</p>
          </div>
        </div>
      </div>

      <div className="max-w-4xl mx-auto px-4 py-6">
        <div className="bg-white rounded-lg shadow-lg p-6">
          {error && (
            <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg text-red-800">
              {error}
            </div>
          )}

          {transaction && (
            <div className="space-y-6">
              <div className="border-b border-gray-200 pb-4">
                <h2 className="text-2xl font-bold text-gray-900">Transaction Information</h2>
              </div>

              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <div className="bg-gray-50 p-4 rounded-lg">
                  <label className="block text-sm font-medium text-gray-600 mb-1">Transaction ID</label>
                  <p className="text-lg font-semibold text-gray-900">{transaction.transactionId}</p>
                </div>

                <div className="bg-gray-50 p-4 rounded-lg">
                  <label className="block text-sm font-medium text-gray-600 mb-1">Transaction Date</label>
                  <p className="text-lg font-semibold text-gray-900">{transaction.transactionDate}</p>
                </div>

                <div className="bg-gray-50 p-4 rounded-lg md:col-span-2">
                  <label className="block text-sm font-medium text-gray-600 mb-1">Description</label>
                  <p className="text-lg font-semibold text-gray-900">{transaction.transactionDescription}</p>
                </div>

                <div className="bg-green-50 p-4 rounded-lg border-2 border-green-200">
                  <label className="block text-sm font-medium text-green-700 mb-1">Amount</label>
                  <p className="text-2xl font-bold text-green-900">{formatAmount(transaction.transactionAmount)}</p>
                </div>

                <div className="bg-gray-50 p-4 rounded-lg">
                  <label className="block text-sm font-medium text-gray-600 mb-1">User</label>
                  <p className="text-lg font-semibold text-gray-900">{transaction.userFullName}</p>
                  <p className="text-sm text-gray-600 mt-1">User ID: {transaction.userId}</p>
                </div>

                <div className="bg-gray-50 p-4 rounded-lg md:col-span-2">
                  <label className="block text-sm font-medium text-gray-600 mb-1">Created At</label>
                  <p className="text-lg font-semibold text-gray-900">{transaction.createdAt}</p>
                </div>
              </div>
            </div>
          )}

          <div className="mt-8 pt-6 border-t border-gray-200 flex gap-4">
            <Button variant="secondary" onClick={() => router.push('/transactions')}>
              PF3 - Return to List
            </Button>
            <Button variant="secondary" onClick={() => router.push('/menu')}>
              PF12 - Return to Menu
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
