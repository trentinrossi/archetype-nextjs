'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard } from '@/types/creditCard';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button, Input } from '@/components/ui';

export default function CreditCardsListPage() {
  const router = useRouter();
  const [creditCards, setCreditCards] = useState<CreditCard[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [accountFilter, setAccountFilter] = useState('');
  const [cardFilter, setCardFilter] = useState('');
  const [page, setPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const [filterError, setFilterError] = useState<{ account?: string; card?: string }>({});
  const pageSize = 10;

  const fetchCreditCards = useCallback(async (currentPage: number = 0) => {
    try {
      setLoading(true);
      setError(null);
      setFilterError({});

      const params: any = {
        page: currentPage,
        size: pageSize,
      };

      if (accountFilter) {
        if (!/^\d{11}$/.test(accountFilter)) {
          setFilterError({ account: 'Account number must be 11 digits' });
          setLoading(false);
          return;
        }
        params.accountId = accountFilter;
      }

      if (cardFilter) {
        if (!/^\d{16}$/.test(cardFilter)) {
          setFilterError({ card: 'Card number must be 16 digits' });
          setLoading(false);
          return;
        }
        params.cardNumber = cardFilter;
      }

      const data = await creditCardService.getCreditCards(params);
      setCreditCards(data.content || []);
      setTotalPages(data.totalPages || 0);
      setTotalElements(data.totalElements || 0);
      setPage(currentPage);
    } catch (err: any) {
      setError(err.message || 'Failed to load credit cards');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, [accountFilter, cardFilter]);

  useEffect(() => {
    fetchCreditCards(0);
  }, [fetchCreditCards]);

  const handleApplyFilters = () => {
    fetchCreditCards(0);
  };

  const handleClearFilters = () => {
    setAccountFilter('');
    setCardFilter('');
    setFilterError({});
  };

  const handleNextPage = () => {
    if (page < totalPages - 1) {
      fetchCreditCards(page + 1);
    }
  };

  const handlePreviousPage = () => {
    if (page > 0) {
      fetchCreditCards(page - 1);
    }
  };

  const handleViewDetails = (cardNumber: string) => {
    router.push(`/credit-cards/${cardNumber}`);
  };

  const handleEdit = (cardNumber: string) => {
    router.push(`/credit-cards/${cardNumber}/edit`);
  };

  const handleDelete = async (cardNumber: string) => {
    if (!confirm('Are you sure you want to delete this credit card?')) return;

    try {
      await creditCardService.deleteCreditCard(cardNumber);
      fetchCreditCards(page);
    } catch (err: any) {
      alert(err.message || 'Failed to delete credit card');
      console.error(err);
    }
  };

  if (loading && creditCards.length === 0) {
    return <div className="p-6">Loading credit cards...</div>;
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Credit Card List</h1>
          <p className="text-sm text-gray-600 mt-1">
            Page {page + 1} of {totalPages || 1} | Total: {totalElements} cards
          </p>
        </div>
        <Button onClick={() => router.push('/credit-cards/new')}>
          Create Credit Card
        </Button>
      </div>

      <div className="bg-white shadow rounded-lg p-4 mb-6">
        <h2 className="text-lg font-semibold mb-4">Filter Criteria</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <Input
              label="Account Number (11 digits)"
              value={accountFilter}
              onChange={(e) => setAccountFilter(e.target.value)}
              placeholder="Enter 11-digit account number"
              maxLength={11}
            />
            {filterError.account && (
              <p className="text-red-600 text-sm mt-1">{filterError.account}</p>
            )}
          </div>
          <div>
            <Input
              label="Card Number (16 digits)"
              value={cardFilter}
              onChange={(e) => setCardFilter(e.target.value)}
              placeholder="Enter 16-digit card number"
              maxLength={16}
            />
            {filterError.card && (
              <p className="text-red-600 text-sm mt-1">{filterError.card}</p>
            )}
          </div>
        </div>
        <div className="flex gap-2 mt-4">
          <Button onClick={handleApplyFilters}>Apply Filters</Button>
          <Button variant="secondary" onClick={handleClearFilters}>
            Clear Filters
          </Button>
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
              <TableHead>Account Number</TableHead>
              <TableHead>Card Number</TableHead>
              <TableHead>Embossed Name</TableHead>
              <TableHead>Expiration</TableHead>
              <TableHead>Status</TableHead>
              <TableHead>Actions</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {creditCards.length === 0 ? (
              <TableRow>
                <TableCell colSpan={6} className="text-center text-gray-500 py-8">
                  No credit cards found
                </TableCell>
              </TableRow>
            ) : (
              creditCards.map((card) => (
                <TableRow key={card.cardNumber}>
                  <TableCell>
                    <div 
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(card.cardNumber)}
                    >
                      {card.accountId}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div 
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(card.cardNumber)}
                    >
                      {card.maskedCardNumber}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div 
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(card.cardNumber)}
                    >
                      {card.embossedName}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div 
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(card.cardNumber)}
                    >
                      {card.expirationDateFormatted}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div 
                      className="cursor-pointer hover:text-blue-600"
                      onClick={() => handleViewDetails(card.cardNumber)}
                    >
                      <span
                        className={`px-2 py-1 rounded text-xs font-semibold ${
                          card.isActive
                            ? 'bg-green-100 text-green-800'
                            : 'bg-gray-100 text-gray-800'
                        }`}
                      >
                        {card.isActive ? 'Active' : 'Inactive'}
                      </span>
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="flex gap-2">
                      <Button
                        size="sm"
                        onClick={(e) => {
                          e.stopPropagation();
                          handleViewDetails(card.cardNumber);
                        }}
                      >
                        View
                      </Button>
                      <Button
                        size="sm"
                        onClick={(e) => {
                          e.stopPropagation();
                          handleEdit(card.cardNumber);
                        }}
                      >
                        Edit
                      </Button>
                      <Button
                        size="sm"
                        variant="danger"
                        onClick={(e) => {
                          e.stopPropagation();
                          handleDelete(card.cardNumber);
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
