'use client';

import React, { useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { AdminMenu } from '@/components/AdminMenu';

export default function AdminPage() {
  const router = useRouter();

  // Check if user is authorized to access admin menu
  useEffect(() => {
    const userType = sessionStorage.getItem('userType');
    if (userType !== 'ADMIN') {
      router.push('/signon');
    }
  }, [router]);

  // COADM01C business rule: Handle admin menu selections
  const handleMenuSelect = (option: string) => {
    switch (option) {
      case 'user-list':
        router.push('/admin/users');
        break;
      case 'user-add':
        router.push('/admin/users/add');
        break;
      case 'user-update':
        router.push('/admin/users/update');
        break;
      case 'user-delete':
        router.push('/admin/users/delete');
        break;
      default:
        console.log('Menu option not implemented:', option);
    }
  };

  // Handle sign out
  const handleSignOut = () => {
    sessionStorage.removeItem('userType');
    sessionStorage.removeItem('redirectProgram');
    router.push('/signon');
  };

  return (
    <AdminMenu
      onMenuSelect={handleMenuSelect}
      onSignOut={handleSignOut}
    />
  );
}