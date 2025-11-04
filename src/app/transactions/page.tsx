'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/transaction';
import { Button, Input } from '@/components/ui';

export default function TransactionListPage() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [hasNext, setHasNext] = useState(false);
  const [hasPrevious, setHasPrevious] = useState(false);
  const [startTransactionId, setStartTransactionId] = useState('');
  const [selectedTransaction, setSelectedTransaction] = useState('');
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
    fetchTransactions();
  }, [currentPage, startTransactionId]);

  const fetchTransactions = async () => {
    try {
      setLoading(true);
      setError('');
      const response = await transactionService.getTransactions(currentPage, 10, startTransactionId);
      setTransactions(response.content);
      setTotalPages(response.totalPages);
      setHasNext(response.hasNext);
      setHasPrevious(response.hasPrevious);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load transactions');
    } finally {
      setLoading(false);
    }
  };

  const handleSelection = (transactionId: string, value: string) => {
    const upper = value.toUpperCase();
    if (upper === 'S' || upper === '') {
      setSelectedTransaction(upper === 'S' ? transactionId : '');
      setError('');
    } else {
      setError('Invalid selection. Valid value is S');
    }
  };

  const handleViewDetails = () => {
    if (selectedTransaction) {
      router.push(`/transactions/${selectedTransaction}`);
    } else {
      setError('Please select a transaction');
    }
  };

  const handlePrevPage = () => {
    if (hasPrevious) {
      setCurrentPage(currentPage - 1);
    } else {
      setError('You have reached the top of the page...');
    }
  };

  const handleNextPage = () => {
    if (hasNext) {
      setCurrentPage(currentPage + 1);
    } else {
      setError('You have reached the bottom of the page...');
    }
  };

  const formatAmount = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-green-600 to-teal-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CT00 - COTRN00C</h1>
              <p className="text-green-100 text-sm">Transaction List</p>
            </div>
            <p className="text-sm">{currentDateTime}</p>
          </div>
        </div>
      </div>

      <div className="max-w-7xl mx-auto px-4 py-6">
        <div className="bg-white rounded-lg shadow-lg p-6">
          <div className="mb-6 flex justify-between items-center">
            <div className="flex gap-4 items-end">
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Filter by Transaction ID
                </label>
                <Input
                  type="text"
                  value={startTransactionId}
                  onChange={(e) => setStartTransactionId(e.target.value)}
                  placeholder="Enter Transaction ID"
                  className="w-64"
                />
              </div>
              <Button onClick={() => setCurrentPage(0)}>Search</Button>
            </div>
            <div className="text-sm text-gray-600">
              Page {currentPage + 1} of {totalPages || 1}
            </div>
          </div>

          {error && (
            <div className="mb-4 p-4 bg-red-50 border border-red-200 rounded-lg text-red-800">
              {error}
            </div>
          )}

          {loading ? (
            <div className="text-center py-12">
              <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-green-600 mx-auto"></div>
              <p className="mt-4 text-gray-600">Loading transactions...</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Sel</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Transaction ID</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Date</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Description</th>
                    <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase">Amount</th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {transactions.map((transaction) => (
                    <tr key={transaction.transactionId} className="hover:bg-gray-50">
                      <td className="px-6 py-4">
                        <Input
                          type="text"
                          value={selectedTransaction === transaction.transactionId ? 'S' : ''}
                          onChange={(e) => handleSelection(transaction.transactionId, e.target.value)}
                          className="w-12 text-center uppercase"
                          maxLength={1}
                        />
                      </td>
                      <td className="px-6 py-4 text-sm font-medium text-gray-900">{transaction.transactionId}</td>
                      <td className="px-6 py-4 text-sm text-gray-900">{transaction.transactionDate}</td>
                      <td className="px-6 py-4 text-sm text-gray-900">{transaction.transactionDescription}</td>
                      <td className="px-6 py-4 text-sm text-right font-medium text-gray-900">
                        {formatAmount(transaction.transactionAmount)}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          <div className="mt-6 flex justify-between items-center">
            <Button variant="secondary" onClick={() => router.push('/menu')}>
              PF3 - Return to Menu
            </Button>
            <div className="flex gap-2">
              <Button variant="secondary" onClick={handlePrevPage} disabled={!hasPrevious}>
                PF7 - Previous
              </Button>
              <Button onClick={handleViewDetails} disabled={!selectedTransaction}>
                View Details
              </Button>
              <Button variant="secondary" onClick={handleNextPage} disabled={!hasNext}>
                PF8 - Next
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
