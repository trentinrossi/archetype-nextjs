import { ReactNode } from 'react';

interface TableProps {
  children: ReactNode;
}

interface TableHeaderProps {
  children: ReactNode;
}

interface TableBodyProps {
  children: ReactNode;
}

interface TableRowProps {
  children: ReactNode;
  className?: string;
}

interface TableCellProps {
  children: ReactNode;
  className?: string;
  colSpan?: number;
}

interface TableHeadProps {
  children: ReactNode;
  className?: string;
}

export function Table({ children }: TableProps) {
  return (
    <div className="w-full overflow-auto">
      <table className="w-full caption-bottom text-sm">
        {children}
      </table>
    </div>
  );
}

export function TableHeader({ children }: TableHeaderProps) {
  return <thead className="[&_tr]:border-b">{children}</thead>;
}

export function TableBody({ children }: TableBodyProps) {
  return <tbody className="[&_tr:last-child]:border-0">{children}</tbody>;
}

export function TableRow({ children, className = '' }: TableRowProps) {
  return (
    <tr className={`border-b border-border transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted ${className}`}>
      {children}
    </tr>
  );
}

export function TableHead({ children, className = '' }: TableHeadProps) {
  return (
    <th className={`h-12 px-4 text-left align-middle font-medium text-muted-foreground [&:has([role=checkbox])]:pr-0 ${className}`}>
      {children}
    </th>
  );
}

export function TableCell({ children, className = '', colSpan }: TableCellProps) {
  return (
    <td className={`p-4 align-middle [&:has([role=checkbox])]:pr-0 ${className}`} colSpan={colSpan}>
      {children}
    </td>
  );
}