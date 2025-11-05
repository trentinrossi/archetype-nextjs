'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Table, Button, Input } from '@/components/ui';
import { Transaction } from '@/types/transaction';
import { transactionService } from '@/services/transactionService';

interface TransactionListProps {
  onDelete?: (id: number) => void;
}

const TransactionList: React.FC<TransactionListProps> = ({ onDelete }) => {
  const router = useRouter();
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(1);
  const [cardNumberFilter, setCardNumberFilter] = useState('');
  const [startDateFilter, setStartDateFilter] = useState('');
  const [endDateFilter, setEndDateFilter] = useState('');
  const itemsPerPage = 20;

  useEffect(() => {
    fetchTransactions();
  }, [currentPage]);

  const fetchTransactions = async () => {
    try {
      setLoading(true);
      setError(null);

      let response;
      if (cardNumberFilter) {
        response = await transactionService.getTransactionsByCardNumber(cardNumberFilter, currentPage, itemsPerPage);
      } else if (startDateFilter && endDateFilter) {
        response = await transactionService.getTransactionsByDateRange(startDateFilter, endDateFilter, currentPage, itemsPerPage);
      } else {
        response = await transactionService.getTransactions(currentPage, itemsPerPage);
      }

      setTransactions(response.content || []);
      setTotalPages(response.totalPages || 1);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load transactions');
      console.error('Error fetching transactions:', err);
      setTransactions([]);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (id: number) => {
    if (!confirm('Are you sure you want to delete this transaction?')) return;

    try {
      await transactionService.deleteTransaction(id);
      if (onDelete) {
        onDelete(id);
      }
      fetchTransactions();
    } catch (err) {
      alert('Failed to delete transaction');
      console.error('Error deleting transaction:', err);
    }
  };

  const handleView = (id: number) => {
    router.push(`/transactions/${id}`);
  };

  const handleFilterApply = () => {
    setCurrentPage(0);
    fetchTransactions();
  };

  const handleFilterReset = () => {
    setCardNumberFilter('');
    setStartDateFilter('');
    setEndDateFilter('');
    setCurrentPage(0);
    fetchTransactions();
  };

  const formatAmount = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatTimestamp = (timestamp: string): string => {
    return new Date(timestamp).toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center p-12">
        <div className="text-center">
          <div className="inline-block h-8 w-8 animate-spin rounded-full border-4 border-solid border-current border-r-transparent"></div>
          <p className="mt-4 text-gray-600">Loading transactions...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
              </svg>
            </div>
            <div className="ml-3">
              <h3 className="text-sm font-medium text-red-800">Error loading transactions</h3>
              <p className="mt-1 text-sm text-red-700">{error}</p>
            </div>
          </div>
          <div className="mt-4">
            <Button variant="secondary" onClick={fetchTransactions}>
              Try Again
            </Button>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      <div className="bg-white shadow rounded-lg p-6">
        <h3 className="text-lg font-semibold mb-4">Filters</h3>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Input
            label="Card Number"
            placeholder="Enter card number"
            value={cardNumberFilter}
            onChange={(e) => setCardNumberFilter(e.target.value)}
          />
          <Input
            label="Start Date"
            type="datetime-local"
            value={startDateFilter}
            onChange={(e) => setStartDateFilter(e.target.value)}
          />
          <Input
            label="End Date"
            type="datetime-local"
            value={endDateFilter}
            onChange={(e) => setEndDateFilter(e.target.value)}
          />
        </div>
        <div className="flex gap-2 mt-4">
          <Button onClick={handleFilterApply}>Apply Filters</Button>
          <Button variant="secondary" onClick={handleFilterReset}>
            Reset
          </Button>
        </div>
      </div>

      {transactions.length === 0 ? (
        <div className="bg-white shadow rounded-lg p-12">
          <div className="text-center">
            <svg
              className="mx-auto h-12 w-12 text-gray-400"
              fill="none"
              viewBox="0 0 24 24"
              stroke="currentColor"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
              />
            </svg>
            <h3 className="mt-2 text-sm font-medium text-gray-900">No transactions found</h3>
            <p className="mt-1 text-sm text-gray-500">
              {cardNumberFilter || startDateFilter || endDateFilter
                ? 'Try adjusting your filters to find transactions.'
                : 'Get started by creating a new transaction.'}
            </p>
          </div>
        </div>
      ) : (
        <>
          <div className="bg-white shadow rounded-lg overflow-hidden">
            <Table
              columns={[
                { key: 'transactionId', label: 'Transaction ID' },
                { key: 'cardNumber', label: 'Card Number' },
                { key: 'typeCode', label: 'Type' },
                { key: 'categoryCode', label: 'Category' },
                { 
                  key: 'amount', 
                  label: 'Amount',
                  render: (value) => formatAmount(value as number)
                },
                { key: 'merchantName', label: 'Merchant' },
                { 
                  key: 'originalTimestamp', 
                  label: 'Date',
                  render: (value) => formatTimestamp(value as string)
                },
              ]}
              data={transactions}
              actions={(transaction) => (
                <div className="flex gap-2">
                  <Button
                    size="sm"
                    onClick={(e) => {
                      e.stopPropagation();
                      handleView(transaction.id);
                    }}
                  >
                    View
                  </Button>
                  <Button
                    size="sm"
                    variant="danger"
                    onClick={(e) => {
                      e.stopPropagation();
                      handleDelete(transaction.id);
                    }}
                  >
                    Delete
                  </Button>
                </div>
              )}
            />
          </div>

          {totalPages > 1 && (
            <div className="bg-white shadow rounded-lg p-4">
              <div className="flex items-center justify-between">
                <div className="text-sm text-gray-700">
                  Page {currentPage + 1} of {totalPages}
                </div>
                <div className="flex gap-2">
                  <Button
                    variant="secondary"
                    size="sm"
                    onClick={() => setCurrentPage(currentPage - 1)}
                    disabled={currentPage === 0}
                  >
                    Previous
                  </Button>
                  <Button
                    variant="secondary"
                    size="sm"
                    onClick={() => setCurrentPage(currentPage + 1)}
                    disabled={currentPage >= totalPages - 1}
                  >
                    Next
                  </Button>
                </div>
              </div>
            </div>
          )}
        </>
      )}
    </div>
  );
};

export default TransactionList;
