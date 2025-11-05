'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/cardServices';
import { Input, Button, Table } from '@/components/ui';

export default function TransactionsPage() {
  const router = useRouter();
  const [searchType, setSearchType] = useState<'transactionId' | 'cardNumber'>('transactionId');
  const [searchValue, setSearchValue] = useState('');
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [hasSearched, setHasSearched] = useState(false);

  const handleSearch = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!searchValue) {
      setError('Please enter a search value');
      return;
    }

    if (searchType === 'transactionId' && searchValue.length !== 16) {
      setError('Transaction ID must be exactly 16 digits');
      return;
    }

    if (searchType === 'cardNumber' && searchValue.length !== 16) {
      setError('Card Number must be exactly 16 digits');
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setHasSearched(true);

      if (searchType === 'transactionId') {
        const transaction = await transactionService.getTransactionById(searchValue);
        setTransactions([transaction]);
      } else {
        const transactionList = await transactionService.getTransactionsByCardNumber(searchValue);
        setTransactions(transactionList);
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to fetch transactions');
      setTransactions([]);
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setSearchValue('');
    setTransactions([]);
    setError(null);
    setHasSearched(false);
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDateTime = (dateString: string): string => {
    return new Date(dateString).toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
    });
  };

  const maskCardNumber = (cardNumber: string): string => {
    if (cardNumber.length !== 16) return cardNumber;
    return `****-****-****-${cardNumber.slice(-4)}`;
  };

  const getTransactionTypeBadge = (typeCode: string): JSX.Element => {
    const typeMap: Record<string, { label: string; color: string }> = {
      '00': { label: 'Purchase', color: 'bg-blue-100 text-blue-800' },
      '01': { label: 'Cash Advance', color: 'bg-purple-100 text-purple-800' },
      '02': { label: 'Return', color: 'bg-green-100 text-green-800' },
      '03': { label: 'Payment', color: 'bg-green-100 text-green-800' },
      '04': { label: 'Fee', color: 'bg-orange-100 text-orange-800' },
      '05': { label: 'Interest', color: 'bg-red-100 text-red-800' },
    };

    const type = typeMap[typeCode] || { label: typeCode, color: 'bg-gray-100 text-gray-800' };

    return (
      <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${type.color}`}>
        {type.label}
      </span>
    );
  };

  const getAmountColor = (amount: number, typeCode: string): string => {
    if (typeCode === '02' || typeCode === '03') {
      return 'text-green-600';
    }
    return 'text-red-600';
  };

  const formatAmount = (amount: number, typeCode: string): string => {
    const formattedAmount = formatCurrency(Math.abs(amount));
    if (typeCode === '02' || typeCode === '03') {
      return `+${formattedAmount}`;
    }
    return `-${formattedAmount}`;
  };

  return (
    <div className="p-6 max-w-7xl mx-auto">
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Transaction History</h1>
        <p className="text-sm text-gray-600 mt-1">Search and view transaction details by Transaction ID or Card Number</p>
      </div>

      <div className="bg-white rounded-lg shadow p-6 mb-6">
        <h2 className="text-lg font-semibold text-gray-900 mb-4">Search Transactions</h2>
        <form onSubmit={handleSearch} className="space-y-4">
          <div className="flex gap-4">
            <div className="flex items-center">
              <input
                type="radio"
                id="transactionId"
                name="searchType"
                value="transactionId"
                checked={searchType === 'transactionId'}
                onChange={(e) => {
                  setSearchType(e.target.value as 'transactionId');
                  setSearchValue('');
                  setError(null);
                }}
                className="mr-2"
              />
              <label htmlFor="transactionId" className="text-sm font-medium text-gray-700">
                Transaction ID
              </label>
            </div>
            <div className="flex items-center">
              <input
                type="radio"
                id="cardNumber"
                name="searchType"
                value="cardNumber"
                checked={searchType === 'cardNumber'}
                onChange={(e) => {
                  setSearchType(e.target.value as 'cardNumber');
                  setSearchValue('');
                  setError(null);
                }}
                className="mr-2"
              />
              <label htmlFor="cardNumber" className="text-sm font-medium text-gray-700">
                Card Number
              </label>
            </div>
          </div>

          <Input
            label={searchType === 'transactionId' ? 'Transaction ID' : 'Card Number'}
            placeholder={searchType === 'transactionId' ? 'Enter 16-digit transaction ID' : 'Enter 16-digit card number'}
            value={searchValue}
            onChange={(e) => {
              const value = e.target.value.replace(/\D/g, '').slice(0, 16);
              setSearchValue(value);
              setError(null);
            }}
            required
            maxLength={16}
          />

          <div className="flex gap-2">
            <Button type="submit" disabled={loading || searchValue.length !== 16}>
              {loading ? 'Searching...' : 'Search'}
            </Button>
            {(searchValue || hasSearched) && (
              <Button type="button" variant="secondary" onClick={handleClear}>
                Clear
              </Button>
            )}
          </div>
        </form>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 rounded-lg p-4 mb-6">
          <div className="flex items-center">
            <span className="text-red-600 text-xl mr-3">⚠️</span>
            <div>
              <h3 className="text-red-800 font-semibold">Error</h3>
              <p className="text-red-600">{error}</p>
            </div>
          </div>
        </div>
      )}

      {loading && (
        <div className="bg-white rounded-lg shadow p-6">
          <div className="flex items-center justify-center py-12">
            <div className="text-center">
              <div className="inline-block animate-spin rounded-full h-8 w-8 border-b-2 border-gray-900 mb-4"></div>
              <p className="text-gray-600">Loading transactions...</p>
            </div>
          </div>
        </div>
      )}

      {!loading && hasSearched && transactions.length === 0 && !error && (
        <div className="bg-white rounded-lg shadow p-6">
          <div className="text-center py-12">
            <span className="text-gray-400 text-5xl mb-4 block">📭</span>
            <h3 className="text-lg font-semibold text-gray-900 mb-2">No Transactions Found</h3>
            <p className="text-gray-600">
              {searchType === 'transactionId'
                ? 'No transaction found with this Transaction ID.'
                : 'No transactions found for this Card Number.'}
            </p>
          </div>
        </div>
      )}

      {!loading && transactions.length > 0 && (
        <div className="bg-white rounded-lg shadow overflow-hidden">
          <div className="px-6 py-4 border-b border-gray-200">
            <h2 className="text-lg font-semibold text-gray-900">
              Transaction Results ({transactions.length})
            </h2>
          </div>
          <div className="overflow-x-auto">
            <Table
              columns={[
                {
                  key: 'transactionId',
                  label: 'Transaction ID',
                  render: (transaction: Transaction) => (
                    <span className="font-mono text-sm">{transaction.transactionId}</span>
                  ),
                },
                {
                  key: 'originationTimestamp',
                  label: 'Date/Time',
                  render: (transaction: Transaction) => (
                    <span className="text-sm">{formatDateTime(transaction.originationTimestamp)}</span>
                  ),
                },
                {
                  key: 'description',
                  label: 'Description',
                  render: (transaction: Transaction) => (
                    <div>
                      <div className="font-medium text-sm">{transaction.description}</div>
                      <div className="text-xs text-gray-500">{transaction.categoryCode}</div>
                    </div>
                  ),
                },
                {
                  key: 'merchantName',
                  label: 'Merchant',
                  render: (transaction: Transaction) => (
                    <div>
                      <div className="font-medium text-sm">{transaction.merchantName}</div>
                      <div className="text-xs text-gray-500">
                        {transaction.merchantCity}, {transaction.merchantZip}
                      </div>
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
                  label: 'Card Number',
                  render: (transaction: Transaction) => (
                    <span className="font-mono text-sm">{maskCardNumber(transaction.cardNumber)}</span>
                  ),
                },
                {
                  key: 'typeCode',
                  label: 'Type',
                  render: (transaction: Transaction) => getTransactionTypeBadge(transaction.typeCode),
                },
                {
                  key: 'amount',
                  label: 'Amount',
                  render: (transaction: Transaction) => (
                    <span className={`font-semibold text-sm ${getAmountColor(transaction.amount, transaction.typeCode)}`}>
                      {formatAmount(transaction.amount, transaction.typeCode)}
                    </span>
                  ),
                },
              ]}
              data={transactions}
              onRowClick={(transaction) => router.push(`/transactions/${transaction.transactionId}`)}
            />
          </div>
        </div>
      )}
    </div>
  );
}
