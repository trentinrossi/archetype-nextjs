'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { cardCrossReferenceService } from '@/services/cardCrossReferenceService';
import { CardCrossReference } from '@/types/card-cross-reference';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button } from '@/components/ui';

export default function CardCrossReferencesPage() {
  const router = useRouter();
  const [cardCrossReferences, setCardCrossReferences] = useState<CardCrossReference[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const pageSize = 20;

  const fetchCardCrossReferences = useCallback(async (page: number = 0) => {
    try {
      setLoading(true);
      const data = await cardCrossReferenceService.getCardCrossReferences(page, pageSize, 'createdAt,desc');
      setCardCrossReferences(data.content);
      setCurrentPage(data.number);
      setTotalPages(data.totalPages);
      setTotalElements(data.totalElements);
      setError(null);
    } catch (err) {
      setError('Failed to load card cross-references');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchCardCrossReferences(currentPage);
  }, [fetchCardCrossReferences, currentPage]);

  const handleDelete = async (accountId: string, cardNumber: string) => {
    if (!confirm(`Are you sure you want to delete the link between account ${accountId} and card ${cardNumber}?`)) return;
    
    try {
      await cardCrossReferenceService.deleteCardCrossReference(accountId, cardNumber);
      fetchCardCrossReferences(currentPage);
    } catch (err) {
      alert('Failed to delete card cross-reference');
      console.error(err);
    }
  };

  const handlePreviousPage = () => {
    if (currentPage > 0) {
      setCurrentPage(currentPage - 1);
    }
  };

  const handleNextPage = () => {
    if (currentPage < totalPages - 1) {
      setCurrentPage(currentPage + 1);
    }
  };

  if (loading && cardCrossReferences.length === 0) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Card Cross References</h1>
          <p className="text-gray-600 mt-1">
            Showing {cardCrossReferences.length} of {totalElements} card-account links
          </p>
        </div>
        <Button onClick={() => router.push('/card-cross-references/new')}>
          Create Link
        </Button>
      </div>
      
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>Account ID</TableHead>
            <TableHead>Card Number</TableHead>
            <TableHead>Created At</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {cardCrossReferences.length === 0 ? (
            <TableRow>
              <TableCell colSpan={4} className="text-center text-gray-500 py-8">
                No card cross-references found
              </TableCell>
            </TableRow>
          ) : (
            cardCrossReferences.map((ref) => (
              <TableRow key={`${ref.accountId}-${ref.cardNumber}`}>
                <TableCell>
                  <div 
                    className="cursor-pointer font-medium text-blue-600 hover:text-blue-800" 
                    onClick={() => router.push(`/accounts/${ref.accountId}`)}
                  >
                    {ref.accountId}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="font-mono">
                    {ref.cardNumber}
                  </div>
                </TableCell>
                <TableCell>
                  {new Date(ref.createdAt).toLocaleString()}
                </TableCell>
                <TableCell>
                  <Button 
                    size="sm" 
                    variant="danger" 
                    onClick={(e) => {
                      e.stopPropagation();
                      handleDelete(ref.accountId, ref.cardNumber);
                    }}
                  >
                    Delete
                  </Button>
                </TableCell>
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>

      {totalPages > 1 && (
        <div className="flex justify-between items-center mt-6">
          <Button 
            variant="secondary" 
            onClick={handlePreviousPage} 
            disabled={currentPage === 0}
          >
            Previous
          </Button>
          <span className="text-gray-600">
            Page {currentPage + 1} of {totalPages}
          </span>
          <Button 
            variant="secondary" 
            onClick={handleNextPage} 
            disabled={currentPage >= totalPages - 1}
          >
            Next
          </Button>
        </div>
      )}
    </div>
  );
}
