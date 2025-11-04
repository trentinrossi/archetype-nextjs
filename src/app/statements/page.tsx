'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { AccountStatement } from '@/types/cardDemo';
import { statementService } from '@/services/statementService';

export default function StatementsPage() {
  const router = useRouter();
  const [statements, setStatements] = useState<AccountStatement[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState<string>('');
  const [filteredStatements, setFilteredStatements] = useState<AccountStatement[]>([]);

  useEffect(() => {
    fetchStatements();
  }, []);

  useEffect(() => {
    applyFilters();
  }, [statements, searchTerm]);

  const fetchStatements = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await statementService.getAllStatements();
      setStatements(data);
      setFilteredStatements(data);
    } catch (err) {
      setError('Failed to load statements');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const applyFilters = () => {
    let filtered = [...statements];

    if (searchTerm) {
      filtered = filtered.filter(
        (statement) =>
          statement.account.accountId.toLowerCase().includes(searchTerm.toLowerCase()) ||
          statement.customer.customerId.toLowerCase().includes(searchTerm.toLowerCase()) ||
          statement.customer.firstName.toLowerCase().includes(searchTerm.toLowerCase()) ||
          statement.customer.lastName.toLowerCase().includes(searchTerm.toLowerCase())
      );
    }

    setFilteredStatements(filtered);
  };

  const formatCurrency = (value: string) => {
    const num = parseFloat(value);
    return isNaN(num) ? value : `$${num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Generating statements...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <p className="text-red-800 font-medium">Error: {error}</p>
          <button
            onClick={fetchStatements}
            className="mt-4 px-4 py-2 bg-red-600 text-white rounded hover:bg-red-700"
          >
            Retry
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Statements</h1>
        <button
          onClick={fetchStatements}
          className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700"
        >
          Refresh Statements
        </button>
      </div>

      <div className="bg-white shadow rounded-lg p-4 mb-6">
        <input
          type="text"
          placeholder="Search by Account ID, Customer ID, or Customer Name"
          value={searchTerm}
          onChange={(e) => setSearchTerm(e.target.value)}
          className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
        />
        <div className="mt-4 text-sm text-gray-600">
          Showing {filteredStatements.length} of {statements.length} statements
        </div>
      </div>

      {filteredStatements.length === 0 ? (
        <div className="bg-gray-50 border border-gray-200 rounded-lg p-8 text-center">
          <p className="text-gray-600">No statements found.</p>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {filteredStatements.map((statement, index) => (
            <div
              key={`${statement.account.accountId}-${index}`}
              className="bg-white shadow rounded-lg p-6 hover:shadow-lg transition-shadow cursor-pointer"
              onClick={() => router.push(`/statements/${statement.account.accountId}`)}
            >
              <div className="flex justify-between items-start mb-4">
                <div>
                  <h3 className="text-lg font-semibold text-gray-900">
                    {statement.customer.firstName} {statement.customer.lastName}
                  </h3>
                  <p className="text-sm text-gray-500">Customer ID: {statement.customer.customerId}</p>
                </div>
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium bg-blue-100 text-blue-800">
                  {statement.account.activeStatus}
                </span>
              </div>

              <div className="space-y-3">
                <div>
                  <label className="block text-xs font-medium text-gray-500 mb-1">Account ID</label>
                  <p className="text-sm font-medium text-gray-900">{statement.account.accountId}</p>
                </div>

                <div>
                  <label className="block text-xs font-medium text-gray-500 mb-1">Current Balance</label>
                  <p className="text-lg font-bold text-gray-900">
                    {formatCurrency(statement.account.currentBalance)}
                  </p>
                </div>

                <div>
                  <label className="block text-xs font-medium text-gray-500 mb-1">Total Transactions</label>
                  <p className="text-sm font-medium text-gray-900">{statement.transactions.length}</p>
                </div>

                <div>
                  <label className="block text-xs font-medium text-gray-500 mb-1">Total Amount</label>
                  <p className="text-lg font-bold text-green-600">
                    {formatCurrency(statement.totalAmount)}
                  </p>
                </div>
              </div>

              <div className="mt-4 pt-4 border-t border-gray-200">
                <button
                  onClick={(e) => {
                    e.stopPropagation();
                    router.push(`/statements/${statement.account.accountId}`);
                  }}
                  className="w-full px-4 py-2 bg-blue-600 text-white text-sm rounded hover:bg-blue-700"
                >
                  View Full Statement
                </button>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  );
}
