'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/account';

const TransactionDetailPage: React.FC = () => {
  const params = useParams();
  const router = useRouter();
  const transactionId = params.id as string;
  
  const [transaction, setTransaction] = useState<Transaction | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchTransaction = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await transactionService.getTransactionById(transactionId);
        setTransaction(data);
      } catch (err) {
        console.error('Error fetching transaction:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch transaction');
      } finally {
        setLoading(false);
      }
    };

    if (transactionId) {
      fetchTransaction();
    }
  }, [transactionId]);

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
      month: 'long',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
    });
  };

  const formatCardNumber = (cardNum: string): string => {
    if (!cardNum) return 'N/A';
    const cleaned = cardNum.replace(/\s/g, '');
    return cleaned.match(/.{1,4}/g)?.join(' ') || cardNum;
  };

  const getTransactionTypeLabel = (code: string): string => {
    const types: Record<string, string> = {
      'PURCHASE': 'Purchase',
      'PAYMENT': 'Payment',
      'REFUND': 'Refund',
      'CASH_ADVANCE': 'Cash Advance',
      'FEE': 'Fee',
      'INTEREST': 'Interest',
      'ADJUSTMENT': 'Adjustment',
    };
    return types[code] || code;
  };

  const getTransactionCategoryLabel = (code: string): string => {
    const categories: Record<string, string> = {
      'RETAIL': 'Retail',
      'GROCERY': 'Grocery',
      'DINING': 'Dining',
      'TRAVEL': 'Travel',
      'GAS': 'Gas/Fuel',
      'ENTERTAINMENT': 'Entertainment',
      'UTILITIES': 'Utilities',
      'HEALTHCARE': 'Healthcare',
      'OTHER': 'Other',
    };
    return categories[code] || code;
  };

  const getTransactionTypeColor = (typeCode: string): string => {
    if (typeCode === 'PAYMENT' || typeCode === 'REFUND') {
      return 'text-green-600';
    } else if (typeCode === 'FEE' || typeCode === 'INTEREST') {
      return 'text-red-600';
    }
    return 'text-gray-900';
  };

  const getTransactionIcon = (typeCode: string): React.ReactElement => {
    if (typeCode === 'PAYMENT' || typeCode === 'REFUND') {
      return (
        <svg className="w-6 h-6 text-green-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 4v16m8-8H4" />
        </svg>
      );
    } else if (typeCode === 'PURCHASE') {
      return (
        <svg className="w-6 h-6 text-blue-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M16 11V7a4 4 0 00-8 0v4M5 9h14l1 12H4L5 9z" />
        </svg>
      );
    } else {
      return (
        <svg className="w-6 h-6 text-gray-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8c-1.657 0-3 .895-3 2s1.343 2 3 2 3 .895 3 2-1.343 2-3 2m0-8c1.11 0 2.08.402 2.599 1M12 8V7m0 1v8m0 0v1m0-1c-1.11 0-2.08-.402-2.599-1M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
        </svg>
      );
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading transaction details...</p>
        </div>
      </div>
    );
  }

  if (error || !transaction) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error || 'Transaction not found'}</p>
          <Button onClick={() => router.push('/transactions')}>Back to Transactions</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      {/* Header */}
      <div className="mb-8">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-900 mb-2">Transaction Details</h1>
            <p className="text-gray-600">Transaction ID: {transaction.transactionId}</p>
          </div>
          <div className="flex gap-3">
            <Button variant="secondary" onClick={() => router.push('/transactions')}>
              Back to List
            </Button>
            <Button onClick={() => router.push(`/transactions/${transactionId}/edit`)}>
              Edit Transaction
            </Button>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Main Content */}
        <div className="lg:col-span-2 space-y-6">
          {/* Transaction Summary Card */}
          <div className="bg-gradient-to-br from-blue-600 to-blue-800 text-white shadow-xl rounded-lg p-8">
            <div className="flex items-center justify-between mb-6">
              <div className="flex items-center gap-4">
                <div className="bg-white/20 p-3 rounded-lg">
                  {getTransactionIcon(transaction.transactionTypeCode)}
                </div>
                <div>
                  <div className="text-sm text-blue-100">Transaction Type</div>
                  <div className="text-xl font-semibold">{getTransactionTypeLabel(transaction.transactionTypeCode)}</div>
                </div>
              </div>
              <div className="text-right">
                <div className="text-sm text-blue-100">Amount</div>
                <div className={`text-3xl font-bold ${transaction.transactionAmount < 0 ? 'text-green-300' : 'text-white'}`}>
                  {formatCurrency(Math.abs(transaction.transactionAmount))}
                </div>
                {transaction.transactionAmount < 0 && (
                  <div className="text-xs text-green-300 mt-1">Credit</div>
                )}
              </div>
            </div>
            <div className="grid grid-cols-2 gap-4 pt-6 border-t border-blue-400/30">
              <div>
                <div className="text-xs text-blue-200">Category</div>
                <div className="text-sm font-medium">{getTransactionCategoryLabel(transaction.transactionCategoryCode)}</div>
              </div>
              <div>
                <div className="text-xs text-blue-200">Source</div>
                <div className="text-sm font-medium">{transaction.transactionSource}</div>
              </div>
            </div>
          </div>

          {/* Transaction Details */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Transaction Information</h2>
            </div>
            <div className="p-6 grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Transaction ID</label>
                <p className="text-base text-gray-900 font-mono">{transaction.transactionId}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Card Number</label>
                <p className="text-base text-gray-900">
                  <button
                    onClick={() => router.push(`/cards/${transaction.cardNumber}`)}
                    className="text-blue-600 hover:text-blue-800 hover:underline font-mono"
                  >
                    {formatCardNumber(transaction.cardNumber)}
                  </button>
                </p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Account ID</label>
                <p className="text-base text-gray-900">
                  <button
                    onClick={() => router.push(`/accounts/${transaction.accountId}`)}
                    className="text-blue-600 hover:text-blue-800 hover:underline"
                  >
                    {transaction.accountId}
                  </button>
                </p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Amount</label>
                <p className={`text-base font-semibold ${getTransactionTypeColor(transaction.transactionTypeCode)}`}>
                  {formatCurrency(transaction.transactionAmount)}
                </p>
              </div>
              <div className="md:col-span-2">
                <label className="block text-sm font-medium text-gray-500 mb-1">Description</label>
                <p className="text-base text-gray-900">{transaction.transactionDescription || 'N/A'}</p>
              </div>
            </div>
          </div>

          {/* Merchant Information */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Merchant Information</h2>
            </div>
            <div className="p-6 grid grid-cols-1 md:grid-cols-2 gap-6">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Merchant Name</label>
                <p className="text-base text-gray-900 font-medium">{transaction.merchantName || 'N/A'}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">Merchant ID</label>
                <p className="text-base text-gray-900 font-mono">{transaction.merchantId || 'N/A'}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">City</label>
                <p className="text-base text-gray-900">{transaction.merchantCity || 'N/A'}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">ZIP Code</label>
                <p className="text-base text-gray-900">{transaction.merchantZip || 'N/A'}</p>
              </div>
            </div>
          </div>

          {/* Timestamps */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Timeline</h2>
            </div>
            <div className="p-6 space-y-4">
              <div className="flex items-start gap-4">
                <div className="bg-blue-100 p-2 rounded-lg">
                  <svg className="w-5 h-5 text-blue-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
                  </svg>
                </div>
                <div className="flex-1">
                  <label className="block text-sm font-medium text-gray-500 mb-1">Original Timestamp</label>
                  <p className="text-base text-gray-900">{formatDateTime(transaction.originalTimestamp)}</p>
                  <p className="text-xs text-gray-500 mt-1">When the transaction was initiated</p>
                </div>
              </div>
              <div className="flex items-start gap-4">
                <div className="bg-green-100 p-2 rounded-lg">
                  <svg className="w-5 h-5 text-green-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
                  </svg>
                </div>
                <div className="flex-1">
                  <label className="block text-sm font-medium text-gray-500 mb-1">Processing Timestamp</label>
                  <p className="text-base text-gray-900">{formatDateTime(transaction.processingTimestamp)}</p>
                  <p className="text-xs text-gray-500 mt-1">When the transaction was processed by our system</p>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Sidebar */}
        <div className="lg:col-span-1 space-y-6">
          {/* Quick Stats */}
          <div className="bg-white shadow-md rounded-lg overflow-hidden">
            <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
              <h2 className="text-xl font-semibold text-gray-900">Quick Info</h2>
            </div>
            <div className="p-6 space-y-4">
              <div className="flex justify-between items-center pb-4 border-b border-gray-200">
                <span className="text-sm text-gray-600">Type</span>
                <span className="text-sm font-medium text-gray-900">{getTransactionTypeLabel(transaction.transactionTypeCode)}</span>
              </div>
              <div className="flex justify-between items-center pb-4 border-b border-gray-200">
                <span className="text-sm text-gray-600">Category</span>
                <span className="text-sm font-medium text-gray-900">{getTransactionCategoryLabel(transaction.transactionCategoryCode)}</span>
              </div>
              <div className="flex justify-between items-center">
                <span className="text-sm text-gray-600">Source</span>
                <span className="text-sm font-medium text-gray-900">{transaction.transactionSource}</span>
              </div>
            </div>
          </div>

          {/* Quick Actions */}
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <h3 className="text-sm font-medium text-blue-900 mb-3">Quick Actions</h3>
            <div className="space-y-2">
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/cards/${transaction.cardNumber}`)}
              >
                View Card
              </Button>
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/accounts/${transaction.accountId}`)}
              >
                View Account
              </Button>
              <Button
                variant="secondary"
                size="sm"
                className="w-full justify-start"
                onClick={() => router.push(`/transactions?card=${transaction.cardNumber}`)}
              >
                All Card Transactions
              </Button>
            </div>
          </div>

          {/* Transaction Note */}
          <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-yellow-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-yellow-800">Important Note</h3>
                <div className="mt-2 text-sm text-yellow-700">
                  <p>Transactions are generally immutable for audit purposes. Only authorized personnel can make corrections.</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default TransactionDetailPage;
