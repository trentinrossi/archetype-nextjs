'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { AccountStatement } from '@/types/cardDemo';
import { statementService } from '@/services/statementService';

export default function StatementDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [statements, setStatements] = useState<AccountStatement[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.accountId) {
      fetchStatements(params.accountId as string);
    }
  }, [params.accountId]);

  const fetchStatements = async (accountId: string) => {
    try {
      setLoading(true);
      setError(null);
      const data = await statementService.getStatementsByAccountId(accountId);
      setStatements(data);
    } catch (err) {
      setError('Failed to load statements');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const formatCurrency = (value: string) => {
    const num = parseFloat(value);
    return isNaN(num) ? value : `$${num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  };

  const formatDate = (dateString: string) => {
    try {
      return new Date(dateString).toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
      });
    } catch {
      return dateString;
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Loading statement...</p>
        </div>
      </div>
    );
  }

  if (error || statements.length === 0) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <p className="text-red-800 font-medium">{error || 'Statement not found'}</p>
          <button
            onClick={() => router.push('/statements')}
            className="mt-4 px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700"
          >
            Back to Statements
          </button>
        </div>
      </div>
    );
  }

  const statement = statements[0];

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Account Statement</h1>
          <p className="text-gray-600 mt-1">Account ID: {statement.account.accountId}</p>
        </div>
        <div className="flex gap-2">
          <button
            onClick={() => window.print()}
            className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
          >
            Print Statement
          </button>
          <button
            onClick={() => router.push('/statements')}
            className="px-4 py-2 bg-gray-200 text-gray-700 rounded hover:bg-gray-300"
          >
            Back to Statements
          </button>
        </div>
      </div>

      <div className="space-y-6">
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Customer Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Customer Name</label>
              <p className="text-gray-900">
                {statement.customer.firstName} {statement.customer.middleName} {statement.customer.lastName}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Customer ID</label>
              <p className="text-gray-900">{statement.customer.customerId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Address</label>
              <p className="text-gray-900">
                {statement.customer.addressLine1}
                {statement.customer.addressLine2 && <>, {statement.customer.addressLine2}</>}
                {statement.customer.addressLine3 && <>, {statement.customer.addressLine3}</>}
              </p>
              <p className="text-gray-900">
                {statement.customer.stateCode}, {statement.customer.countryCode} {statement.customer.zipCode}
              </p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Phone</label>
              <p className="text-gray-900">{statement.customer.phoneNumber1}</p>
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Account Summary</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Account ID</label>
              <p className="text-gray-900">{statement.account.accountId}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Current Balance</label>
              <p className="text-2xl font-bold text-gray-900">{formatCurrency(statement.account.currentBalance)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Credit Limit</label>
              <p className="text-gray-900">{formatCurrency(statement.account.creditLimit)}</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Available Credit</label>
              <p className="text-gray-900 text-green-600 font-semibold">
                {formatCurrency(
                  (parseFloat(statement.account.creditLimit) - parseFloat(statement.account.currentBalance)).toString()
                )}
              </p>
            </div>
          </div>
        </div>

        <div className="bg-white shadow rounded-lg p-6">
          <div className="flex justify-between items-center mb-4">
            <h2 className="text-lg font-semibold">Transaction History</h2>
            <p className="text-sm text-gray-600">{statement.transactions.length} transactions</p>
          </div>

          {statement.transactions.length === 0 ? (
            <div className="text-center py-8">
              <p className="text-gray-600">No transactions found for this statement period.</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Date</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Description</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Merchant</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Type</th>
                    <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase">Amount</th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {statement.transactions.map((transaction) => (
                    <tr key={transaction.transactionId}>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {formatDate(transaction.originalTimestamp)}
                      </td>
                      <td className="px-6 py-4 text-sm text-gray-900">{transaction.transactionDescription}</td>
                      <td className="px-6 py-4 text-sm text-gray-900">
                        <div>{transaction.merchantName}</div>
                        <div className="text-xs text-gray-500">{transaction.merchantCity}</div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {transaction.transactionTypeCode}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-right font-medium">
                        <span className={parseFloat(transaction.transactionAmount) < 0 ? 'text-red-600' : 'text-green-600'}>
                          {formatCurrency(transaction.transactionAmount)}
                        </span>
                      </td>
                    </tr>
                  ))}
                </tbody>
                <tfoot className="bg-gray-50">
                  <tr>
                    <td colSpan={4} className="px-6 py-4 text-sm font-semibold text-gray-900 text-right">
                      Total Amount:
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-right font-bold text-gray-900">
                      {formatCurrency(statement.totalAmount)}
                    </td>
                  </tr>
                </tfoot>
              </table>
            </div>
          )}
        </div>

        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <p className="text-sm text-blue-800">
            <strong>Statement Date:</strong> {new Date().toLocaleDateString('en-US', {
              year: 'numeric',
              month: 'long',
              day: 'numeric',
            })}
          </p>
          <p className="text-sm text-blue-800 mt-2">
            This statement shows all transactions and the current balance for account {statement.account.accountId}.
          </p>
        </div>
      </div>
    </div>
  );
}
