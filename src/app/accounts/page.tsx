'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account } from '@/types/account';
import { Table, Button, Select } from '@/components/ui';

export default function AccountsPage() {
  const router = useRouter();
  const [accounts, setAccounts] = useState<Account[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [statusFilter, setStatusFilter] = useState<string>('all');

  useEffect(() => {
    fetchAccounts();
  }, [statusFilter]);

  const fetchAccounts = async () => {
    try {
      setLoading(true);
      setError(null);
      
      let data: Account[];
      if (statusFilter === 'all') {
        data = await accountService.getAccounts();
      } else {
        data = await accountService.getAccountsByStatus(statusFilter);
      }
      
      setAccounts(data);

      if (data.length === 0) {
        setError('No accounts found');
      }
    } catch (err) {
      setError('Failed to load accounts');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (accountId: string) => {
    if (!confirm('Are you sure you want to delete this account? This will also delete all associated cards.')) return;

    try {
      await accountService.deleteAccount(accountId);
      fetchAccounts();
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string) => {
    try {
      return new Date(dateString).toLocaleDateString();
    } catch {
      return dateString;
    }
  };

  return (
    <div className="p-6">
      {/* Header Section */}
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Accounts</h1>
          <p className="text-sm text-gray-600">Account Management</p>
        </div>
        <Button onClick={() => router.push('/accounts/new')}>
          Create Account
        </Button>
      </div>

      {/* Filter Section */}
      <div className="bg-white shadow rounded-lg p-4 mb-6">
        <div className="flex items-end gap-4">
          <div className="flex-1 max-w-xs">
            <Select
              label="Filter by Status"
              value={statusFilter}
              onChange={(e) => setStatusFilter(e.target.value)}
              options={[
                { value: 'all', label: 'All Accounts' },
                { value: 'Y', label: 'Active (Y)' },
                { value: 'N', label: 'Inactive (N)' },
              ]}
            />
          </div>
          <Button variant="secondary" onClick={fetchAccounts}>
            Refresh
          </Button>
        </div>
      </div>

      {/* Messages Section */}
      {error && (
        <div className="bg-yellow-50 border border-yellow-200 text-yellow-800 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}

      {/* Account List Section */}
      {loading ? (
        <div className="text-center py-8">Loading...</div>
      ) : accounts.length > 0 ? (
        <div className="bg-white shadow rounded-lg overflow-hidden">
          <Table
            columns={[
              { key: 'accountId', label: 'Account ID' },
              { key: 'activeStatus', label: 'Status' },
              { key: 'currentBalance', label: 'Balance' },
              { key: 'creditLimit', label: 'Credit Limit' },
              { key: 'availableCredit', label: 'Available Credit' },
              { key: 'openDate', label: 'Open Date' },
            ]}
            data={accounts.map(account => ({
              ...account,
              activeStatus: account.active ? 'Active' : 'Inactive',
              currentBalance: formatCurrency(account.currentBalance),
              creditLimit: formatCurrency(account.creditLimit),
              availableCredit: formatCurrency(account.availableCredit),
              openDate: formatDate(account.openDate),
            }))}
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
      ) : null}

      {/* Summary Section */}
      {accounts.length > 0 && (
        <div className="mt-4 text-sm text-gray-600">
          Showing {accounts.length} account{accounts.length !== 1 ? 's' : ''}
        </div>
      )}

      {/* Navigation Actions */}
      <div className="flex gap-2 mt-6">
        <Button variant="secondary" onClick={() => router.push('/')}>
          Return to Main Menu
        </Button>
      </div>
    </div>
  );
}
