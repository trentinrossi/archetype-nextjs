'use client';

import React, { useEffect, useState } from 'react';
import { Transaction } from '@/types/account';
import { Table, Button, Input, Select } from '@/components/ui';
import { useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';

const TransactionsPage: React.FC = () => {
  const router = useRouter();
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [filteredTransactions, setFilteredTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [sourceFilter, setSourceFilter] = useState<string>('all');

  const fetchTransactions = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await transactionService.getTransactions();
      setTransactions(data);
      setFilteredTransactions(data);
    } catch (err) {
      console.error('Error fetching transactions:', err);
      setError(err instanceof Error ? err.message : 'An unexpected error occurred');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchTransactions();
  }, []);

  useEffect(() => {
    let filtered = [...transactions];

    if (searchTerm.trim() !== '') {
      const term = searchTerm.toLowerCase();
      filtered = filtered.filter(transaction =>
        transaction.transactionId.toLowerCase().includes(term) ||
        transaction.cardNumber.toLowerCase().includes(term) ||
        transaction.accountId.toLowerCase().includes(term) ||
        transaction.transactionDescription.toLowerCase().includes(term)
      );
    }

    if (sourceFilter !== 'all') {
      filtered = filtered.filter(transaction => transaction.transactionSource === sourceFilter);
    }

    setFilteredTransactions(filtered);
  }, [searchTerm, sourceFilter, transactions]);

  const handleViewDetails = (transactionId: string) => {
    router.push(`/transactions/${transactionId}`);
  };

  const handleCreateNew = () => {
    router.push('/transactions/new');
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDateTime = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  const columns = [
    {
      key: 'transactionId',
      header: 'Transaction ID',
      render: (transaction: Transaction) => (
        <span className="font-medium text-gray-900 font-mono text-sm">{transaction.transactionId}</span>
      ),
    },
    {
      key: 'cardNumber',
      header: 'Card Number',
      render: (transaction: Transaction) => (
        <span className="text-gray-900 font-mono">****{transaction.cardNumber.slice(-4)}</span>
      ),
    },
    {
      key: 'accountId',
      header: 'Account ID',
      render: (transaction: Transaction) => (
        <span className="text-gray-900">{transaction.accountId}</span>
      ),
    },
    {
      key: 'description',
      header: 'Description',
      render: (transaction: Transaction) => (
        <div className="max-w-xs">
          <div className="text-gray-900 truncate">{transaction.transactionDescription}</div>
          {transaction.merchantName && (
            <div className="text-xs text-gray-500">{transaction.merchantName}</div>
          )}
        </div>
      ),
    },
    {
      key: 'amount',
      header: 'Amount',
      render: (transaction: Transaction) => (
        <span className={`font-medium ${
          transaction.transactionAmount >= 0 ? 'text-green-600' : 'text-red-600'
        }`}>
          {formatCurrency(transaction.transactionAmount)}
        </span>
      ),
    },
    {
      key: 'source',
      header: 'Source',
      render: (transaction: Transaction) => (
        <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
          {transaction.transactionSource}
        </span>
      ),
    },
    {
      key: 'timestamp',
      header: 'Date & Time',
      render: (transaction: Transaction) => (
        <span className="text-gray-900 text-sm">{formatDateTime(transaction.originalTimestamp)}</span>
      ),
    },
    {
      key: 'actions',
      header: 'Actions',
      render: (transaction: Transaction) => (
        <Button
          variant="secondary"
          size="sm"
          onClick={() => handleViewDetails(transaction.transactionId)}
        >
          View
        </Button>
      ),
    },
  ];

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading transactions...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error}</p>
          <Button onClick={fetchTransactions}>Retry</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Transactions</h1>
        <p className="text-gray-600">View and manage all transaction records</p>
      </div>

      <div className="mb-6 flex flex-wrap gap-4 items-end">
        <div className="flex-1 min-w-[300px]">
          <label htmlFor="search" className="block text-sm font-medium text-gray-700 mb-1">
            Search Transactions
          </label>
          <Input
            id="search"
            type="text"
            placeholder="Search by ID, card number, account, or description..."
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
          />
        </div>

        <div className="flex-1 min-w-[200px]">
          <label htmlFor="source-filter" className="block text-sm font-medium text-gray-700 mb-1">
            Filter by Source
          </label>
          <Select
            id="source-filter"
            value={sourceFilter}
            onChange={(e) => setSourceFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Sources' },
              { value: 'System', label: 'System' },
              { value: 'POS', label: 'POS' },
              { value: 'Online', label: 'Online' },
              { value: 'ATM', label: 'ATM' },
            ]}
          />
        </div>

        <div className="flex-shrink-0">
          <Button onClick={handleCreateNew}>Create New Transaction</Button>
        </div>
      </div>

      {filteredTransactions.length === 0 ? (
        <div className="text-center py-12 bg-gray-50 rounded-lg">
          <svg
            className="mx-auto h-12 w-12 text-gray-400"
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            aria-hidden="true"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-3 7h3m-3 4h3m-6-4h.01M9 16h.01"
            />
          </svg>
          <h3 className="mt-2 text-sm font-medium text-gray-900">No transactions found</h3>
          <p className="mt-1 text-sm text-gray-500">
            {transactions.length === 0
              ? 'No transactions have been recorded yet.'
              : 'Try adjusting your search or filters to see more results.'}
          </p>
        </div>
      ) : (
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <Table
            data={filteredTransactions}
            columns={columns}
            keyExtractor={(transaction) => transaction.transactionId}
          />
          <div className="px-6 py-4 bg-gray-50 border-t border-gray-200">
            <p className="text-sm text-gray-700">
              Showing <span className="font-medium">{filteredTransactions.length}</span> of{' '}
              <span className="font-medium">{transactions.length}</span> transactions
            </p>
          </div>
        </div>
      )}
    </div>
  );
};

export default TransactionsPage;
