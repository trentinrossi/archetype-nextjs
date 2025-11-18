'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { Account } from '@/types/account';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button, Input } from '@/components/ui';

export default function AccountsPage() {
  const router = useRouter();
  const [accounts, setAccounts] = useState<Account[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [page, setPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const pageSize = 20;

  const fetchAccounts = useCallback(async (currentPage: number = 0, search?: string) => {
    try {
      setLoading(true);
      setError(null);

      const params: any = {
        page: currentPage,
        size: pageSize,
      };

      if (search) {
        params.searchTerm = search;
      }

      const data = await accountService.getAccounts(params);
      setAccounts(data.content || []);
      setTotalPages(data.totalPages || 0);
      setTotalElements(data.totalElements || 0);
      setPage(currentPage);
    } catch (err: any) {
      setError(err.message || 'Failed to load accounts');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchAccounts(0);
  }, [fetchAccounts]);

  const handleSearch = () => {
    if (searchTerm.trim()) {
      fetchAccounts(0, searchTerm);
    } else {
      fetchAccounts(0);
    }
  };

  const handleClearSearch = () => {
    setSearchTerm('');
    fetchAccounts(0);
  };

  const handleNextPage = () => {
    if (page < totalPages - 1) {
      fetchAccounts(page + 1, searchTerm || undefined);
    }
  };

  const handlePreviousPage = () => {
    if (page > 0) {
      fetchAccounts(page - 1, searchTerm || undefined);
    }
  };

  const handleViewDetails = (accountId: number) => {
    router.push(`/accounts/${accountId}`);
  };

  const handleViewCards = (accountId: number) => {
    router.push(`/credit-cards?accountId=${accountId}`);
  };

  const handleDelete = async (accountId: number) => {
    if (!confirm('Are you sure you want to delete this account?')) return;

    try {
      await accountService.deleteAccount(accountId.toString());
      fetchAccounts(page, searchTerm || undefined);
    } catch (err: any) {
      alert(err.message || 'Failed to delete account');
      console.error(err);
    }
  };

  if (loading && accounts.length === 0) {
    return <div className="p-6">Loading accounts...</div>;
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Accounts</h1>
          <p className="text-sm text-gray-600 mt-1">
            Page {page + 1} of {totalPages || 1} | Total: {totalElements} accounts
          </p>
        </div>
        <Button onClick={() => router.push('/accounts/new')}>
          Create Account
        </Button>
      </div>

      <div className="bg-white shadow rounded-lg p-4 mb-6">
        <h2 className="text-lg font-semibold mb-4">Search Accounts</h2>
        <div className="flex gap-2">
          <div className="flex-1">
            <Input
              label=""
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              placeholder="Search by account ID..."
              onKeyPress={(e) => {
                if (e.key === 'Enter') {
                  handleSearch();
                }
              }}
            />
          </div>
          <div className="flex gap-2 items-end">
            <Button onClick={handleSearch}>Search</Button>
            <Button variant="secondary" onClick={handleClearSearch}>
              Clear
            </Button>
          </div>
        </div>
      </div>

      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}

      <div className="bg-white shadow rounded-lg overflow-hidden">
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Account ID</TableHead>
              <TableHead>Formatted Account ID</TableHead>
              <TableHead>Credit Cards</TableHead>
              <TableHead>Created At</TableHead>
              <TableHead>Actions</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {accounts.length === 0 ? (
              <TableRow>
                <TableCell colSpan={5} className="text-center text-gray-500 py-8">
                  No accounts found
                </TableCell>
              </TableRow>
            ) : (
              accounts.map((account) => (
                <TableRow key={account.accountId}>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(account.accountId)}
                    >
                      {account.accountId}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(account.accountId)}
                    >
                      {account.formattedAccountId}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(account.accountId)}
                    >
                      {account.creditCardCount}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(account.accountId)}
                    >
                      {new Date(account.createdAt).toLocaleDateString()}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="flex gap-2">
                      <Button
                        size="sm"
                        onClick={(e) => {
                          e.stopPropagation();
                          handleViewCards(account.accountId);
                        }}
                      >
                        View Cards
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
                  </TableCell>
                </TableRow>
              ))
            )}
          </TableBody>
        </Table>
      </div>

      <div className="flex justify-between items-center mt-4">
        <Button
          variant="secondary"
          onClick={handlePreviousPage}
          disabled={page === 0}
        >
          Previous Page
        </Button>
        <span className="text-sm text-gray-600">
          Page {page + 1} of {totalPages || 1}
        </span>
        <Button
          variant="secondary"
          onClick={handleNextPage}
          disabled={page >= totalPages - 1}
        >
          Next Page
        </Button>
      </div>
    </div>
  );
}
