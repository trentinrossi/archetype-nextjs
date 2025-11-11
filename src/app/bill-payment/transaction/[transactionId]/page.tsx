'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { billPaymentService } from '@/services/billPaymentService';
import { Transaction } from '@/types/bill-payment';
import { Button } from '@/components/ui';

export default function TransactionDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [transaction, setTransaction] = useState<Transaction | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchTransaction = useCallback(async (transactionId: number) => {
    try {
      setLoading(true);
      const data = await billPaymentService.getTransactionById(transactionId);
      setTransaction(data);
      setError(null);
    } catch (err: any) {
      setError(err.message || 'Failed to load transaction details');
      setTransaction(null);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    if (params.transactionId) {
      fetchTransaction(Number(params.transactionId));
    }
  }, [params.transactionId, fetchTransaction]);

  if (loading) {
    return (
      <div className="p-6 max-w-4xl mx-auto">
        <div className="text-center py-12">
          <div className="text-gray-500">Loading transaction details...</div>
        </div>
      </div>
    );
  }

  if (error || !transaction) {
    return (
      <div className="p-6 max-w-4xl mx-auto">
        <div className="mb-6">
          <h1 className="text-3xl font-bold mb-4">Transaction Details</h1>
        </div>
        <div className="bg-red-50 border border-red-200 rounded-lg p-6">
          <p className="text-red-800 font-semibold">
            {error || 'Transaction not found'}
          </p>
        </div>
        <div className="mt-6">
          <Button variant="secondary" onClick={() => router.push('/bill-payment/history')}>
            Back to History
          </Button>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl mx-auto">
      <div className="mb-6">
        <div className="flex justify-between items-center mb-4">
          <h1 className="text-3xl font-bold">Transaction Details</h1>
          <div className="flex gap-2">
            <Button variant="secondary" onClick={() => router.push('/bill-payment/history')}>
              Back to History
            </Button>
            <Button variant="secondary" onClick={() => router.push('/bill-payment')}>
              New Payment
            </Button>
          </div>
        </div>
      </div>

      {/* Transaction Status Banner */}
      <div className="mb-6 bg-green-50 border border-green-200 rounded-lg p-4">
        <div className="flex items-center justify-between">
          <div>
            <p className="text-green-800 font-semibold text-lg">Payment Completed Successfully</p>
            <p className="text-green-700 text-sm mt-1">
              Transaction ID: {transaction.transactionId}
            </p>
          </div>
          <span className="px-4 py-2 rounded-lg text-sm font-semibold bg-green-100 text-green-800">
            COMPLETED
          </span>
        </div>
      </div>

      {/* Transaction Information */}
      <div className="bg-white shadow rounded-lg p-6 space-y-6">
        <div>
          <h2 className="text-xl font-semibold mb-4 pb-2 border-b">Transaction Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Transaction ID
              </label>
              <p className="text-gray-900 font-mono">{transaction.transactionId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Amount
              </label>
              <p className="text-gray-900 text-xl font-bold text-green-600">
                ${transaction.amount.toFixed(2)}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Transaction Type
              </label>
              <p className="text-gray-900">
                {transaction.transactionTypeCode} - Bill Payment
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Category
              </label>
              <p className="text-gray-900">
                {transaction.transactionCategoryCode} - Payment
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Transaction Source
              </label>
              <p className="text-gray-900">{transaction.transactionSource}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Description
              </label>
              <p className="text-gray-900">{transaction.description}</p>
            </div>
          </div>
        </div>

        <div>
          <h2 className="text-xl font-semibold mb-4 pb-2 border-b">Account & Card Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Account ID
              </label>
              <p className="text-gray-900 font-mono">{transaction.accountId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Number
              </label>
              <p className="text-gray-900 font-mono">
                {transaction.cardNumber.replace(/(\d{4})(\d{4})(\d{4})(\d{4})/, '$1 $2 $3 $4')}
              </p>
            </div>
          </div>
        </div>

        <div>
          <h2 className="text-xl font-semibold mb-4 pb-2 border-b">Merchant Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Merchant ID
              </label>
              <p className="text-gray-900">{transaction.merchantId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Merchant Name
              </label>
              <p className="text-gray-900">{transaction.merchantName}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Merchant City
              </label>
              <p className="text-gray-900">{transaction.merchantCity}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Merchant ZIP
              </label>
              <p className="text-gray-900">{transaction.merchantZip}</p>
            </div>
          </div>
        </div>

        <div>
          <h2 className="text-xl font-semibold mb-4 pb-2 border-b">Timestamps</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Origination Time
              </label>
              <p className="text-gray-900">
                {new Date(transaction.originationTimestamp).toLocaleString()}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Processing Time
              </label>
              <p className="text-gray-900">
                {new Date(transaction.processingTimestamp).toLocaleString()}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Created At
              </label>
              <p className="text-gray-900">
                {new Date(transaction.createdAt).toLocaleString()}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Updated At
              </label>
              <p className="text-gray-900">
                {new Date(transaction.updatedAt).toLocaleString()}
              </p>
            </div>
          </div>
        </div>
      </div>

      {/* Print/Export Actions */}
      <div className="mt-6 bg-gray-50 border border-gray-200 rounded-lg p-4">
        <div className="flex justify-between items-center">
          <p className="text-sm text-gray-600">
            Keep this transaction ID for your records: <span className="font-mono font-semibold">{transaction.transactionId}</span>
          </p>
          <Button
            variant="secondary"
            size="sm"
            onClick={() => window.print()}
          >
            Print Receipt
          </Button>
        </div>
      </div>
    </div>
  );
}
