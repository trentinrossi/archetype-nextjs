'use client';

import React, { useEffect, useState } from 'react';
import { AccountStatement } from '@/types/account';
import { Button, Input } from '@/components/ui';
import { useRouter } from 'next/navigation';
import { statementService } from '@/services/statementService';

const StatementsPage: React.FC = () => {
  const router = useRouter();
  const [statements, setStatements] = useState<AccountStatement[]>([]);
  const [filteredStatements, setFilteredStatements] = useState<AccountStatement[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState('');

  const fetchStatements = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await statementService.getAllStatements();
      setStatements(data);
      setFilteredStatements(data);
    } catch (err) {
      console.error('Error fetching statements:', err);
      setError(err instanceof Error ? err.message : 'An unexpected error occurred');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchStatements();
  }, []);

  useEffect(() => {
    if (searchTerm.trim() === '') {
      setFilteredStatements(statements);
    } else {
      const term = searchTerm.toLowerCase();
      const filtered = statements.filter(statement =>
        statement.account.accountId.toLowerCase().includes(term) ||
        statement.customer.customerId.toLowerCase().includes(term) ||
        `${statement.customer.firstName} ${statement.customer.lastName}`.toLowerCase().includes(term)
      );
      setFilteredStatements(filtered);
    }
  }, [searchTerm, statements]);

  const handleViewStatement = (accountId: string) => {
    router.push(`/statements/${accountId}`);
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading statements...</p>
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
          <Button onClick={fetchStatements}>Retry</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Account Statements</h1>
        <p className="text-gray-600">View and generate account statements</p>
      </div>

      <div className="mb-6">
        <label htmlFor="search" className="block text-sm font-medium text-gray-700 mb-1">
          Search Statements
        </label>
        <Input
          id="search"
          type="text"
          placeholder="Search by account ID, customer ID, or name..."
          value={searchTerm}
          onChange={(e) => setSearchTerm(e.target.value)}
        />
      </div>

      {filteredStatements.length === 0 ? (
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
              d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
            />
          </svg>
          <h3 className="mt-2 text-sm font-medium text-gray-900">No statements found</h3>
          <p className="mt-1 text-sm text-gray-500">
            {statements.length === 0
              ? 'No statements have been generated yet.'
              : 'Try adjusting your search to see more results.'}
          </p>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {filteredStatements.map((statement) => (
            <div
              key={statement.account.accountId}
              className="bg-white shadow-md rounded-lg overflow-hidden hover:shadow-lg transition-shadow"
            >
              <div className="p-6">
                <div className="flex items-center justify-between mb-4">
                  <h3 className="text-lg font-semibold text-gray-900">
                    Account {statement.account.accountId}
                  </h3>
                  <span
                    className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                      statement.account.activeStatus === 'Y'
                        ? 'bg-green-100 text-green-800'
                        : 'bg-red-100 text-red-800'
                    }`}
                  >
                    {statement.account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                  </span>
                </div>

                <div className="space-y-3">
                  <div>
                    <p className="text-sm text-gray-500">Customer</p>
                    <p className="text-sm font-medium text-gray-900">
                      {statement.customer.firstName} {statement.customer.lastName}
                    </p>
                    <p className="text-xs text-gray-500">ID: {statement.customer.customerId}</p>
                  </div>

                  <div>
                    <p className="text-sm text-gray-500">Current Balance</p>
                    <p className="text-lg font-bold text-gray-900">
                      {formatCurrency(statement.account.currentBalance)}
                    </p>
                  </div>

                  <div>
                    <p className="text-sm text-gray-500">Total Transactions</p>
                    <p className="text-sm font-medium text-gray-900">
                      {statement.transactions.length} transaction{statement.transactions.length !== 1 ? 's' : ''}
                    </p>
                  </div>

                  <div>
                    <p className="text-sm text-gray-500">Total Amount</p>
                    <p className={`text-sm font-medium ${
                      statement.totalAmount >= 0 ? 'text-green-600' : 'text-red-600'
                    }`}>
                      {formatCurrency(statement.totalAmount)}
                    </p>
                  </div>
                </div>

                <div className="mt-6">
                  <Button
                    onClick={() => handleViewStatement(statement.account.accountId)}
                    className="w-full"
                  >
                    View Statement
                  </Button>
                </div>
              </div>
            </div>
          ))}
        </div>
      )}

      {filteredStatements.length > 0 && (
        <div className="mt-6 text-center">
          <p className="text-sm text-gray-700">
            Showing <span className="font-medium">{filteredStatements.length}</span> of{' '}
            <span className="font-medium">{statements.length}</span> statements
          </p>
        </div>
      )}
    </div>
  );
};

export default StatementsPage;
