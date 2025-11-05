'use client';

import React from 'react';
import { Table, Button } from '@/components/ui';
import { Transaction } from '@/types/user';

interface TransactionListProps {
  transactions: Transaction[];
  loading?: boolean;
  currentPage: number;
  totalPages: number;
  hasNext: boolean;
  hasPrevious: boolean;
  onPageChange: (page: number) => void;
  onTransactionClick?: (transaction: Transaction) => void;
}

export default function TransactionList({
  transactions,
  loading = false,
  currentPage,
  totalPages,
  hasNext,
  hasPrevious,
  onPageChange,
  onTransactionClick,
}: TransactionListProps) {
  const handlePrevious = () => {
    if (hasPrevious) {
      onPageChange(currentPage - 1);
    }
  };

  const handleNext = () => {
    if (hasNext) {
      onPageChange(currentPage + 1);
    }
  };

  const formatDate = (dateString: string): string => {
    const date = new Date(dateString);
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const year = String(date.getFullYear()).slice(-2);
    return `${month}/${day}/${year}`;
  };

  const formatAmount = (amount: number): string => {
    return `$${amount.toFixed(2)}`;
  };

  const handleRowClick = (transaction: Transaction) => {
    if (onTransactionClick) {
      onTransactionClick(transaction);
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center p-8">
        <div className="text-gray-600">Loading transactions...</div>
      </div>
    );
  }

  if (!transactions || transactions.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center p-8 bg-gray-50 rounded-lg border border-gray-200">
        <svg
          className="w-16 h-16 text-gray-400 mb-4"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-3 7h3m-3 4h3m-6-4h.01M9 16h.01"
          />
        </svg>
        <h3 className="text-lg font-semibold text-gray-900 mb-2">No transactions found</h3>
        <p className="text-gray-600 text-center">
          There are no transactions to display.
        </p>
      </div>
    );
  }

  return (
    <div className="space-y-4">
      <Table
        columns={[
          { key: 'transactionId', label: 'Transaction ID' },
          { 
            key: 'transactionDate', 
            label: 'Date',
            render: (value) => formatDate(value as string)
          },
          { key: 'transactionDescription', label: 'Description' },
          { 
            key: 'transactionAmount', 
            label: 'Amount',
            render: (value) => formatAmount(value as number)
          },
          { key: 'userFullName', label: 'User' },
        ]}
        data={transactions}
        onRowClick={onTransactionClick ? handleRowClick : undefined}
      />

      <div className="flex items-center justify-between border-t border-gray-200 bg-white px-4 py-3 sm:px-6 rounded-b-lg">
        <div className="flex flex-1 justify-between sm:hidden">
          <Button
            variant="secondary"
            size="sm"
            onClick={handlePrevious}
            disabled={!hasPrevious}
          >
            Previous
          </Button>
          <Button
            variant="secondary"
            size="sm"
            onClick={handleNext}
            disabled={!hasNext}
          >
            Next
          </Button>
        </div>
        <div className="hidden sm:flex sm:flex-1 sm:items-center sm:justify-between">
          <div>
            <p className="text-sm text-gray-700">
              Page <span className="font-medium">{currentPage + 1}</span> of{' '}
              <span className="font-medium">{totalPages}</span>
            </p>
          </div>
          <div className="flex gap-2">
            <Button
              variant="secondary"
              size="sm"
              onClick={handlePrevious}
              disabled={!hasPrevious}
            >
              Previous
            </Button>
            <Button
              variant="secondary"
              size="sm"
              onClick={handleNext}
              disabled={!hasNext}
            >
              Next
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
