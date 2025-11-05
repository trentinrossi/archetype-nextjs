'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Account } from '@/types/cardServices';
import { accountService } from '@/services/accountService';
import { Table, Button, Input, Select } from '@/components/ui';

export default function AccountsPage() {
  const router = useRouter();
  const [accounts, setAccounts] = useState<Account[]>([]);
  const [filteredAccounts, setFilteredAccounts] = useState<Account[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [statusFilter, setStatusFilter] = useState<'all' | 'Y' | 'N'>('all');
  const [currentPage, setCurrentPage] = useState(1);
  const [itemsPerPage] = useState(10);

  useEffect(() => {
    fetchAccounts();
  }, []);

  useEffect(() => {
    filterAccounts();
  }, [accounts, searchTerm, statusFilter]);

  const fetchAccounts = async () => {
    try {
      setLoading(true);
      const data = await accountService.getAccounts();
      setAccounts(data);
      setError(null);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load accounts');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const fetchAccountsByStatus = async (status: 'Y' | 'N') => {
    try {
      setLoading(true);
      const data = await accountService.getAccountsByStatus(status);
      setAccounts(data);
      setError(null);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load accounts');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const filterAccounts = () => {
    let filtered = [...accounts];

    if (searchTerm) {
      filtered = filtered.filter(
        (account) =>
          account.accountId.toLowerCase().includes(searchTerm.toLowerCase()) ||
          (account.groupId && account.groupId.toLowerCase().includes(searchTerm.toLowerCase()))
      );
    }

    if (statusFilter !== 'all') {
      filtered = filtered.filter((account) => account.activeStatus === statusFilter);
    }

    setFilteredAccounts(filtered);
    setCurrentPage(1);
  };

  const handleStatusFilterChange = (value: string) => {
    const newStatus = value as 'all' | 'Y' | 'N';
    setStatusFilter(newStatus);
    
    if (newStatus === 'all') {
      fetchAccounts();
    } else {
      fetchAccountsByStatus(newStatus);
    }
  };

  const handleDelete = async (accountId: string) => {
    if (!confirm('Are you sure you want to delete this account?')) return;

    try {
      await accountService.deleteAccount(accountId);
      fetchAccounts();
    } catch (err) {
      alert(err instanceof Error ? err.message : 'Failed to delete account');
      console.error(err);
    }
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string): string => {
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const getPaginatedData = () => {
    const startIndex = (currentPage - 1) * itemsPerPage;
    const endIndex = startIndex + itemsPerPage;
    return filteredAccounts.slice(startIndex, endIndex);
  };

  const totalPages = Math.ceil(filteredAccounts.length / itemsPerPage);

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-16 w-16 border-b-2 border-blue-600 mx-auto mb-4"></div>
          <p className="text-gray-600">Loading accounts...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex items-center">
            <span className="text-red-600 text-xl mr-3">⚠️</span>
            <div>
              <h3 className="text-red-800 font-semibold">Error Loading Accounts</h3>
              <p className="text-red-600">{error}</p>
            </div>
          </div>
          <Button variant="secondary" onClick={fetchAccounts} className="mt-4">
            Retry
          </Button>
        </div>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold text-gray-900">Accounts</h1>
          <p className="text-sm text-gray-600 mt-1">
            Manage credit card accounts and balances
          </p>
        </div>
        <Button onClick={() => router.push('/accounts/new')}>Create Account</Button>
      </div>

      <div className="bg-white rounded-lg shadow mb-6 p-4">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div className="md:col-span-2">
            <Input
              label="Search"
              placeholder="Search by Account ID or Group ID..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
            />
          </div>
          <div>
            <Select
              label="Status Filter"
              value={statusFilter}
              onChange={(e) => handleStatusFilterChange(e.target.value)}
              options={[
                { value: 'all', label: 'All Statuses' },
                { value: 'Y', label: 'Active' },
                { value: 'N', label: 'Inactive' },
              ]}
            />
          </div>
        </div>

        <div className="mt-4 flex items-center justify-between text-sm text-gray-600">
          <div>
            Showing {filteredAccounts.length} of {accounts.length} accounts
          </div>
          {statusFilter !== 'all' && (
            <div className="flex items-center">
              <span className="mr-2">Filtered by:</span>
              <span className="bg-blue-100 text-blue-800 px-2 py-1 rounded">
                {statusFilter === 'Y' ? 'Active' : 'Inactive'}
              </span>
            </div>
          )}
        </div>
      </div>

      <div className="bg-white rounded-lg shadow overflow-hidden">
        <Table
          columns={[
            {
              key: 'accountId',
              label: 'Account ID',
              render: (account: Account) => (
                <span className="font-mono text-sm">{account.accountId}</span>
              ),
            },
            {
              key: 'activeStatus',
              label: 'Status',
              render: (account: Account) => (
                <span
                  className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                    account.activeStatus === 'Y'
                      ? 'bg-green-100 text-green-800'
                      : 'bg-red-100 text-red-800'
                  }`}
                >
                  {account.activeStatus === 'Y' ? 'Active' : 'Inactive'}
                </span>
              ),
            },
            {
              key: 'currentBalance',
              label: 'Current Balance',
              render: (account: Account) => (
                <span className="font-semibold">
                  {formatCurrency(account.currentBalance)}
                </span>
              ),
            },
            {
              key: 'creditLimit',
              label: 'Credit Limit',
              render: (account: Account) => formatCurrency(account.creditLimit),
            },
            {
              key: 'availableCredit',
              label: 'Available Credit',
              render: (account: Account) => (
                <span className="text-green-600 font-semibold">
                  {formatCurrency(account.availableCredit)}
                </span>
              ),
            },
            {
              key: 'openDate',
              label: 'Open Date',
              render: (account: Account) => formatDate(account.openDate),
            },
            {
              key: 'expirationDate',
              label: 'Expiration Date',
              render: (account: Account) => (
                <span
                  className={
                    account.expired ? 'text-red-600 font-semibold' : ''
                  }
                >
                  {formatDate(account.expirationDate)}
                  {account.expired && ' (Expired)'}
                </span>
              ),
            },
          ]}
          data={getPaginatedData()}
          onRowClick={(account) => router.push(`/accounts/${account.accountId}`)}
          actions={(account) => (
            <div className="flex gap-2">
              <Button
                size="sm"
                onClick={(e) => {
                  e.stopPropagation();
                  router.push(`/accounts/${account.accountId}`);
                }}
              >
                View
              </Button>
              <Button
                size="sm"
                variant="secondary"
                onClick={(e) => {
                  e.stopPropagation();
                  router.push(`/accounts/${account.accountId}/edit`);
                }}
              >
                Edit
              </Button>
              <Button
                size="sm"
                variant="danger"
                onClick={(e) => {
                  e.stopPropagation();
                  handleDelete(account.accountId);
                }}
              >
                Delete
              </Button>
            </div>
          )}
        />
      </div>

      {totalPages > 1 && (
        <div className="mt-6 flex items-center justify-between">
          <div className="text-sm text-gray-600">
            Page {currentPage} of {totalPages}
          </div>
          <div className="flex gap-2">
            <Button
              variant="secondary"
              size="sm"
              onClick={() => setCurrentPage((prev) => Math.max(prev - 1, 1))}
              disabled={currentPage === 1}
            >
              Previous
            </Button>
            <Button
              variant="secondary"
              size="sm"
              onClick={() =>
                setCurrentPage((prev) => Math.min(prev + 1, totalPages))
              }
              disabled={currentPage === totalPages}
            >
              Next
            </Button>
          </div>
        </div>
      )}
    </div>
  );
}
